-- | Readers for the specification's conformance files, copied byte for byte from tandem-spec
-- conformance/*.json, and the hashes their dumps need. A case is one line of a file, so a case
-- is read by key with no general JSON parser.
module Conformance
  ( Case
  , loadCases
  , hasField
  , text
  , number
  , strings
  , numbers
  , hex
  , caseKey
  , draws
  , dumpDraws
  , fillCuts
  , fails
  , bits64
  , bits32
  , sha256
  , fnv1a
  ) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad.ST (ST, runST)
import Data.Bits (complement, rotateR, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Char (isDigit)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, isJust)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Numeric (showHex)
import Test.Tasty.HUnit (assertFailure)

import System.Random.Tandem

newtype Case = Case BC.ByteString

-- | The cases of a file: its lines that open an object with an id or a file.
loadCases :: FilePath -> IO [Case]
loadCases file = do
  contents <- BC.readFile ("tests/conformance/" ++ file)
  pure [Case l | l <- BC.lines contents, any (`BC.isInfixOf` l) [BC.pack "{\"id\":", BC.pack "{\"file\":"]]

-- | The text that follows the key and its colon, or 'Nothing'.
at :: String -> Case -> Maybe BC.ByteString
at k (Case l) =
  let (_, rest) = BC.breakSubstring (BC.pack ("\"" ++ k ++ "\": ")) l
   in if BC.null rest then Nothing else Just (BC.drop (length k + 4) rest)

hasField :: String -> Case -> Bool
hasField k = isJust . at k

required :: String -> Case -> BC.ByteString
required k c = fromMaybe (error ("no field " ++ k)) (at k c)

-- | A string value.
text :: String -> Case -> String
text k = BC.unpack . BC.takeWhile (/= '"') . BC.drop 1 . required k

number :: String -> Case -> Int
number k c = maybe (error ("no number " ++ k)) fst (BC.readInt (required k c))

-- | The strings of a list value.
strings :: String -> Case -> [String]
strings k c = [BC.unpack s | (i, s) <- zip [0 :: Int ..] (BC.split '"' region), odd i]
  where
    region = BC.takeWhile (/= ']') (required k c)

-- | The integers of a list value.
numbers :: String -> Case -> [Int]
numbers k c = [n | w <- BC.words (BC.map (\ch -> if isDigit ch then ch else ' ') region), Just (n, _) <- [BC.readInt w]]
  where
    region = BC.takeWhile (/= ']') (required k c)

-- | A hexadecimal string as a number.
hex :: String -> Word64
hex = foldl' (\a ch -> a `shiftL` 4 + digit ch) 0
  where
    digit ch
      | isDigit ch = fromIntegral (fromEnum ch - 48)
      | otherwise = fromIntegral (fromEnum ch - 87)

-- | The key of a case.
caseKey :: Case -> Quad
caseKey c = case map (fromIntegral . hex) (strings "key" c) of
  [a, b, d, e] -> Quad a b d e
  _ -> error "key"

-- | The (kind, n) fills of a dump.
dumpDraws :: Case -> [(String, Int)]
dumpDraws c = go (required "draws" c)
  where
    pat = BC.pack "{\"kind\": \""
    go s = case BC.breakSubstring pat s of
      (_, r)
        | BC.null r -> []
        | otherwise ->
            let (kind, r2) = BC.break (== '"') (BC.drop (BC.length pat) r)
                r3 = BC.drop 5 (snd (BC.breakSubstring (BC.pack "\"n\": ") r2))
             in (BC.unpack kind, maybe 0 fst (BC.readInt r3)) : go r3

-- | @n@ scalar draws.
draws :: Int -> (Tandem -> (a, Tandem)) -> Tandem -> ([a], Tandem)
draws n next g0 = let (g1, xs) = mapAccumL (\g _ -> let (x, g') = next g in (g', x)) g0 [1 .. n] in (xs, g1)

-- | One vector from consecutive in-place fills of the given lengths.
fillCuts :: MU.Unbox a => [Int] -> (forall s. MU.MVector s a -> Tandem -> ST s Tandem) -> Tandem -> (U.Vector a, Tandem)
fillCuts cuts fillM g = runST $ do
  m <- MU.new (sum cuts)
  let go _ [] h = pure h
      go i (c : cs) h = fillM (MU.slice i c m) h >>= go (i + c) cs
  h <- go 0 cuts g
  u <- U.unsafeFreeze m
  pure (u, h)

-- | The value is an error.
fails :: a -> IO ()
fails x = do
  r <- try (evaluate x)
  case r of
    Left (_ :: ErrorCall) -> pure ()
    Right _ -> assertFailure "expected an error"

bits64 :: U.Vector Double -> [Word64]
bits64 = map castDoubleToWord64 . U.toList

bits32 :: U.Vector Float -> [Word32]
bits32 = map castFloatToWord32 . U.toList

-- Hashes ----------------------------------------------------------------------------------

-- | The 64-bit FNV-1a hash of the bytes.
fnv1a :: B.ByteString -> Word64
fnv1a = B.foldl' (\h b -> (h `xor` fromIntegral b) * 0x100000001b3) 0xcbf29ce484222325

roundConstants :: U.Vector Word32
roundConstants =
  U.fromList
    [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
    , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
    , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
    , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
    , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
    , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
    , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
    , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
    ]

data State = State !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32

-- | The block of 64 bytes at offset @o@.
compress :: B.ByteString -> Int -> State -> State
compress bs o st0@(State a0 b0 c0 d0 e0 f0 g0 h0) = go 0 st0
  where
    be i = let p = o + 4 * i in
      (fromIntegral (unsafeIndex bs p) `shiftL` 24) .|. (fromIntegral (unsafeIndex bs (p + 1)) `shiftL` 16)
        .|. (fromIntegral (unsafeIndex bs (p + 2)) `shiftL` 8) .|. fromIntegral (unsafeIndex bs (p + 3)) :: Word32
    w = U.constructN 64 $ \prev ->
      let i = U.length prev
       in if i < 16
            then be i
            else
              let x = prev U.! (i - 15)
                  y = prev U.! (i - 2)
                  s0 = rotateR x 7 `xor` rotateR x 18 `xor` (x `shiftR` 3)
                  s1 = rotateR y 17 `xor` rotateR y 19 `xor` (y `shiftR` 10)
               in prev U.! (i - 16) + s0 + prev U.! (i - 7) + s1
    go :: Int -> State -> State
    go 64 (State a b c d e f g h) = State (a0 + a) (b0 + b) (c0 + c) (d0 + d) (e0 + e) (f0 + f) (g0 + g) (h0 + h)
    go i (State a b c d e f g h) =
      let t1 = h + (rotateR e 6 `xor` rotateR e 11 `xor` rotateR e 25) + ((e .&. f) `xor` (complement e .&. g))
                 + U.unsafeIndex roundConstants i + U.unsafeIndex w i
          t2 = (rotateR a 2 `xor` rotateR a 13 `xor` rotateR a 22) + ((a .&. b) `xor` (a .&. c) `xor` (b .&. c))
       in go (i + 1) (State (t1 + t2) a b c (d + t1) e f g)

-- | The SHA-256 of the bytes as 64 lowercase hexadecimal digits.
sha256 :: B.ByteString -> String
sha256 msg = concatMap word [a, b, c, d, e, f, g, h]
  where
    len = B.length msg
    padding = B.concat [B.singleton 0x80, B.replicate ((55 - len) `mod` 64) 0, B.pack [fromIntegral ((fromIntegral len * 8 :: Word64) `shiftR` (8 * k)) | k <- [7, 6 .. 0 :: Int]]]
    full = len - len `mod` 64
    State a b c d e f g h =
      let st = foldl' (\s o -> compress msg o s) init0 [0, 64 .. full - 64]
          rest = B.append (B.drop full msg) padding
       in foldl' (\s o -> compress rest o s) st [0, 64 .. B.length rest - 64]
    init0 = State 0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a 0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19
    word x = let s = showHex x "" in replicate (8 - length s) '0' ++ s
