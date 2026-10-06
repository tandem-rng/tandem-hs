-- | The conformance files of the specification, tests/conformance, and every behaviour of its
-- CHECKLIST.md that this port offers: the bounded, normal, exponential and weighted choice
-- fixtures with their scalar draws and cut fills, the stream and dump hashes, and the position
-- bounds. The port has no Bool, 128-bit, binary16, Char or complex draws, so it checks the
-- complex streams as real fills of twice the length and leaves the others out.
module Checklist (checklist) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.ST (ST)
import Data.Bifunctor (first)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe, isNothing)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord64ToDouble)
import System.Random qualified as R
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import Conformance
import System.Random.Tandem

checklist :: TestTree
checklist =
  testGroup
    "conformance"
    [ testGroup
        "cases"
        [ cases "below.json" 11
        , cases "fill_below.json" 77
        , cases "normal.json" 20
        , cases "exponential.json" 12
        , cases "choice.json" 24
        ]
    , testCase "fallback by global draw index" fallbackIndex
    , testCase "width from range" widthFromRange
    , testCase "empty fills" emptyFills
    , testCase "Float normals: odd n and the pair rule" floatNormals
    , testCase "weighted choice" weightedChoice
    , testCase "weighted choice rejects invalid weights" invalidWeights
    , testCase "weighted choice follows its weights" choiceLaw
    , testCase "stream hashes" streamHashes
    , testCase "dump hashes" dumpHashes
    , testCase "random access equals the sequential fill" randomAccess
    , testCase "position bounds" positionBounds
    ]

-- Cases -----------------------------------------------------------------------------------

data Kind = BelowU32 | BelowU64 | FillBelowU32 | FillBelowU64 | NormalF64 | NormalF32 | ExponentialF64 | ExponentialF32 | ChoiceFill
  deriving (Eq)

kindOf :: Case -> Kind
kindOf c = case text "kind" c of
  "below_u32" -> BelowU32
  "below_u64" -> BelowU64
  "fill_below_u32" -> FillBelowU32
  "fill_below_u64" -> FillBelowU64
  "fill_normal_f64" -> NormalF64
  "fill_normal_f32" -> NormalF32
  "fill_exponential_f64" -> ExponentialF64
  "fill_exponential_f32" -> ExponentialF32
  "fill_choice" -> ChoiceFill
  k -> error ("unknown kind " ++ k)

generator :: Case -> Tandem
generator c = fromKey (caseKey c) (fromIntegral (number "start" c)) (fromIntegral (number "K" c))

table :: Case -> Choice
table c = validTable (map (castWord64ToDouble . hex) (strings "weights" c))

-- | A fill as bit patterns, whole and in pieces.
data Fill = forall a. U.Unbox a => Fill (a -> Word64) (Int -> Tandem -> (U.Vector a, Tandem)) (forall s. MU.MVector s a -> Tandem -> ST s Tandem)

fillOf :: Case -> Fill
fillOf c = case kindOf c of
  FillBelowU32 -> let r = fromIntegral (hex (text "range" c)) in Fill fromIntegral (fillBelow32 r) (fillBelow32M r)
  FillBelowU64 -> let r = hex (text "range" c) in Fill id (fillBelow64 r) (fillBelow64M r)
  NormalF64 -> Fill castDoubleToWord64 fillNormal fillNormalM
  NormalF32 -> Fill (fromIntegral . castFloatToWord32) fillNormalFloat fillNormalFloatM
  ExponentialF64 -> Fill castDoubleToWord64 fillExponential fillExponentialM
  ExponentialF32 -> Fill (fromIntegral . castFloatToWord32) fillExponentialFloat fillExponentialFloatM
  ChoiceFill -> let t = table c in Fill fromIntegral (fillChoice t) (fillChoiceM t)
  _ -> error "not a fill"

runFill :: Fill -> Int -> Tandem -> ([Word64], Tandem)
runFill (Fill bits f _) n g = let (v, g') = f n g in (map bits (U.toList v), g')

runCuts :: Fill -> [Int] -> Tandem -> ([Word64], Tandem)
runCuts (Fill bits _ fm) cuts g = let (v, g') = fillCuts cuts fm g in (map bits (U.toList v), g')

-- | @n@ scalar draws of the kind, as bit patterns. A Float normal comes in pairs.
scalarOf :: Case -> Int -> Tandem -> ([Word64], Tandem)
scalarOf c n g = case kindOf c of
  k | k == BelowU32 || k == FillBelowU32 -> first (map fromIntegral) (draws n (nextBelow32 (fromIntegral (hex (text "range" c)))) g)
  k | k == BelowU64 || k == FillBelowU64 -> draws n (nextBelow64 (hex (text "range" c))) g
  NormalF64 -> first (map castDoubleToWord64) (draws n nextNormal g)
  NormalF32 ->
    let (ps, h) = draws ((n + 1) `div` 2) nextNormalPairFloat g
        w = fromIntegral . castFloatToWord32
     in (take n (concatMap (\(a, b) -> [w a, w b]) ps), h)
  ExponentialF64 -> first (map castDoubleToWord64) (draws n nextExponential g)
  ExponentialF32 -> first (map (fromIntegral . castFloatToWord32)) (draws n nextExponentialFloat g)
  _ -> first (map fromIntegral) (draws n (nextChoice (table c)) g)

align :: Int -> Int -> Int
align p w = (p + w - 1) `div` w * w

-- | The position after the operation by the rules of Appendices A and C: an empty bounded, Float
-- normal or exponential fill moves nothing, and any other empty fill aligns.
expectedEnd :: Kind -> Int -> Int -> Int
expectedEnd kind p n = case kind of
  FillBelowU32 -> bounded 32
  ExponentialF32 -> bounded 32
  FillBelowU64 -> bounded 64
  ExponentialF64 -> bounded 64
  NormalF32 -> if n == 0 then p else align p 32 + 64 * ((n + 1) `div` 2)
  _ -> align p 64 + 64 * n
  where
    bounded w = if n == 0 then p else align p w + w * n

-- | The draws below the Lemire threshold in the plain fill: the elements that take the fallback.
rejections :: Case -> Int
rejections c
  | kindOf c == FillBelowU32 =
      let r = toInteger (hex (text "range" c))
          t = (negate r) `mod` (2 ^ (32 :: Int)) `mod` r
       in length [() | x <- U.toList (fst (fillWord32 n (generator c))), toInteger x * r `mod` (2 ^ (32 :: Int)) < t]
  | otherwise =
      let r = toInteger (hex (text "range" c))
          t = (negate r) `mod` (2 ^ (64 :: Int)) `mod` r
       in length [() | x <- U.toList (fst (fillWord64 n (generator c))), toInteger x * r `mod` (2 ^ (64 :: Int)) < t]
  where
    n = number "n" c

-- | Every case of a file: the whole fill, the scalar draws, and fills cut at elements 1, 7, 20,
-- 21 and n - 1 and run in order on one generator. The end positions follow the rules of the
-- appendices, and the file's own @end@ agrees with them.
cases :: String -> Int -> TestTree
cases file count = testCase file $ do
  cs <- loadCases file
  length cs @?= count
  rejecting <- forM cs checkCase
  when (file == "fill_below.json") $ assertBool "some case takes the fallback" (or rejecting)

checkCase :: Case -> IO Bool
checkCase c = do
  let kind = kindOf c
      label = text "id" c
      n = number "n" c
      p = number "start" c
      want = map hex (strings "values" c)
      -- A scalar bounded draw retries on its own stream, so its end is the file's.
      scalarKind = kind == BelowU32 || kind == BelowU64
      stop = if scalarKind then number "end" c else expectedEnd kind p n
      g = generator c
      isBelow = kind == FillBelowU32 || kind == FillBelowU64
      rejected = if isBelow && n > 0 then rejections c else 0
  length want @?= n
  when (hasField "end" c) $ assertEqual label stop (number "end" c)
  when (hasField "rejected" c) $ assertEqual label rejected (number "rejected" c)
  when (kind == ChoiceFill) $ do
    let t = table c
    assertEqual label (hex (text "capacity" c)) (choiceCapacity t)
    when (hasField "cut" c) $ do
      assertEqual label (map hex (strings "cut" c)) (U.toList (choiceCuts t))
      assertEqual label (map hex (strings "alias" c)) (map fromIntegral (U.toList (choiceAliases t)))
  let (got, h) = if scalarKind then scalarOf c n g else runFill (fillOf c) n g
  assertEqual label (want, stop) (got, fromIntegral (position h))
  unless scalarKind $ do
    -- A fill is the scalar draws, except that a rejected bounded draw retries elsewhere.
    when (n > 0 && rejected == 0) $ do
      let (xs, h') = scalarOf c n g
      assertEqual (label ++ " scalar") (want, stop) (xs, fromIntegral (position h'))
    -- A Float normal fill cut at an odd element drops a sin half, so only even cuts compose.
    forM_ [k | k <- [1, 7, 20, 21, n - 1], k > 0, k < n, kind /= NormalF32 || even k] $ \k -> do
      let (v, h') = runCuts (fillOf c) [k, n - k] g
      assertEqual (label ++ " cut at " ++ show k) (want, stop) (v, fromIntegral (position h'))
  pure (rejected > 0)

-- Appendix rules --------------------------------------------------------------------------

valuesOf :: [Case] -> String -> [Word64]
valuesOf cs name = case [c | c <- cs, (' ' : name) `isSuffix` text "id" c] of
  [c] -> map hex (strings "values" c)
  _ -> error ("no case " ++ name)
  where
    isSuffix s t = reverse s == take (length s) (reverse t)

caseOf :: [Case] -> String -> Case
caseOf cs name = case [c | c <- cs, (' ' : name) `isSuffix` text "id" c] of
  [c] -> c
  _ -> error ("no case " ++ name)
  where
    isSuffix s t = reverse s == take (length s) (reverse t)

-- | The table of weights that are known to be valid.
validTable :: [Double] -> Choice
validTable w = fromMaybe (error "weights") (choice (U.fromList w))

key42 :: Quad
key42 = Quad 0x421d21eb 0x32d31777 0x62e7564b 0xdf2bdf82

-- | Element @i@ of a fill from start 1 is element @i + 1@ of the fill from start 0, so the
-- fallback of a rejected draw is keyed by its index in the stream.
fallbackIndex :: IO ()
fallbackIndex = do
  fb <- loadCases "fill_below.json"
  forM_ [("CROSS_BELOW32[4]", "CROSS_BELOW32_AT[4]"), ("CROSS_BELOW64[6]", "CROSS_BELOW64_AT[6]")] $ \(a, b) ->
    drop 1 (valuesOf fb a) @?= take 63 (valuesOf fb b)
  nm <- loadCases "normal.json"
  drop 1 (valuesOf nm "CROSS_NORMAL[0]") @?= take 63 (valuesOf nm "CROSS_NORMAL[1]")
  -- Elements 20 of CROSS_NORMAL[3] to [5] miss, so their cut fills in the cases take the
  -- fallback at and after the cut.
  forM_ [3 .. 5 :: Int] $ \i -> number "n" (caseOf nm ("CROSS_NORMAL[" ++ show i ++ "]")) @?= 64

-- | The bounded draws take their width from the range or the function: range 1000 gives the
-- values of CROSS_BELOW32[3] with 'fillBelow' and 'fillBelow32', and CROSS_BELOW64[3] with
-- 'fillBelow64'. Range 0 returns 0 after one draw of that width.
widthFromRange :: IO ()
widthFromRange = do
  fb <- loadCases "fill_below.json"
  let g = fromKey key42 0 32
      w32 = valuesOf fb "CROSS_BELOW32[3]"
      w64 = valuesOf fb "CROSS_BELOW64[3]"
  map fromIntegral (U.toList (fst (fillBelow 1000 64 g))) @?= w32
  map fromIntegral (U.toList (fst (fillBelow32 1000 64 g))) @?= w32
  map fromIntegral (U.toList (fst (fillBelow64 1000 64 g))) @?= w64
  assertBool "the widths differ" (w32 /= w64)
  forM_ [0, 1, 33] $ \p -> do
    let h = fromKey key42 p 32
        (a, h32) = nextBelow32 0 h
        (b, h64) = nextBelow64 0 h
        (v, hr) = fillBelow 0 3 h
    (a, position h32) @?= (0, fromIntegral (align (fromIntegral p) 32 + 32))
    (b, position h64) @?= (0, fromIntegral (align (fromIntegral p) 64 + 64))
    (U.toList v, position hr) @?= ([0, 0, 0], fromIntegral (align (fromIntegral p) 32 + 96))

-- | The seven cases with n = 0 start at 33. A uniform, Double normal or choice fill aligns, and a
-- bounded, Float normal or exponential fill leaves the position; 'cases' checks each end.
emptyFills :: IO ()
emptyFills = do
  cs <- concat <$> mapM loadCases ["fill_below.json", "normal.json", "exponential.json", "choice.json"]
  let empty = [c | c <- cs, number "n" c == 0]
  length empty @?= 7
  forM_ empty $ \c -> number "start" c @?= 33
  position (snd (fillWord32 0 (seek 33 (seed 1)))) @?= 64
  position (snd (fillFloat 0 (seek 33 (seed 1)))) @?= 64

-- | A Float normal fill of odd n writes the cos half of its last pair and consumes both draws.
-- Element 2j is the cos half and 2j + 1 the sin half of draws 2j and 2j + 1, and a scalar normal
-- is the cos half and consumes two draws.
floatNormals :: IO ()
floatNormals = do
  nm <- loadCases "normal.json"
  let f = valuesOf nm "CROSS_NORMALF"
      n32 i = valuesOf nm ("CROSS_NORMAL32[" ++ show (i :: Int) ++ "]")
  take 33 f @?= n32 1
  drop 2 (n32 0) @?= take 31 (n32 2)
  forM_ [0 .. 4 :: Int] $ \i -> do
    let c = caseOf nm ("CROSS_NORMAL32[" ++ show i ++ "]")
    number "n" c @?= 33
    expectedEnd NormalF32 (number "start" c) 33 @?= align (number "start" c) 32 + 32 * 2 * 17
  position (snd (fillNormalFloat 33 (fromKey key42 0 32))) @?= 1088
  let g = fromKey key42 0 32
      (z, h) = nextNormalFloat g
  ([fromIntegral (castFloatToWord32 z)], position h) @?= (take 1 (n32 0), 64)

-- Weighted choice -------------------------------------------------------------------------

weightedChoice :: IO ()
weightedChoice = do
  cs <- loadCases "choice.json"
  length [() | c <- cs, hasField "cut" c] @?= 5
  let a = valuesOf cs "CROSS_CHOICE[0]"
      s = valuesOf cs "CROSS_CHOICE[1]"
  (number "start" (caseOf cs "CROSS_CHOICE[0]"), number "start" (caseOf cs "CROSS_CHOICE[1]")) @?= (0, 1)
  drop 1 a @?= take (length a - 1) s
  -- m = 1 returns 0 and still consumes 64 bits.
  let (i, g) = nextChoice (validTable [0.25]) (seek 5 (fromKey key42 0 32))
  (i, position g) @?= (0, 128)
  -- A zero weight, written as -0.0 too, never appears.
  assertBool "only the positive weights" (U.all (`elem` [1, 3]) (fst (fillChoice (validTable [-0.0, 3, 0, 1]) 5000 (seed 9))))

invalidWeights :: IO ()
invalidWeights = do
  let inf = 1 / 0 :: Double
      nan = 0 / 0 :: Double
  forM_ [[], [1, -1], [1, nan], [1, inf], [1, -inf], [0, -0.0], [0]] $ \w ->
    assertBool (show w) (isNothing (choice (U.fromList w)))
  assertBool "a subnormal weight" (not (isNothing (choice (U.fromList [5e-324]))))

-- | Chi-square of 10^6 draws against nine positive weights, 8 degrees of freedom: the 0.0005 and
-- 0.9995 quantiles are 0.71 and 27.87. The zero weight never appears.
choiceLaw :: IO ()
choiceLaw = do
  let w = [0.5, 3, 0, 1, 7, 2.25, 0.1, 4, 1, 6] :: [Double]
      n = 1000000
      v = fst (fillChoice (validTable w) n (seed 2028))
      counts = U.accumulate (+) (U.replicate (length w) (0 :: Int)) (U.map (\x -> (fromIntegral x, 1)) v)
      total = sum w
      chi2 = sum [(fromIntegral cnt - e) ^ (2 :: Int) / e | (cnt, wi) <- zip (U.toList counts) w, wi > 0, let e = fromIntegral n * wi / total]
  counts U.! 2 @?= 0
  assertBool ("chi2 " ++ show chi2) (chi2 > 0.71 && chi2 < 27.87)

-- Hashes ----------------------------------------------------------------------------------

lazyBytes :: BB.Builder -> B.ByteString
lazyBytes = BL.toStrict . BB.toLazyByteString

doubles :: U.Vector Double -> BB.Builder
doubles = U.foldr ((<>) . BB.doubleLE) mempty

floats :: U.Vector Float -> BB.Builder
floats = U.foldr ((<>) . BB.floatLE) mempty

-- | The bytes of a uniform stream of n elements, little endian. A complex element is its real
-- and imaginary parts, so a complex fill is the real fill of twice the length.
streamBytes :: String -> Int -> Tandem -> B.ByteString
streamBytes ty n g = case ty of
  "UInt32" -> lazyBytes (U.foldr ((<>) . BB.word32LE) mempty (fst (fillWord32 n g)))
  "UInt64" -> lazyBytes (U.foldr ((<>) . BB.word64LE) mempty (fst (fillWord64 n g)))
  "Float64" -> lazyBytes (doubles (fst (fillDouble n g)))
  "Float32" -> lazyBytes (floats (fst (fillFloat n g)))
  "ComplexF64" -> lazyBytes (doubles (fst (fillDouble (2 * n) g)))
  "ComplexF32" -> lazyBytes (floats (fst (fillFloat (2 * n) g)))
  "UInt8" -> B.pack (fst (draws n R.genWord8 g))
  _ -> error ("unsupported type " ++ ty)

-- | The SHA-256 of every uniform stream of hashes.json this port draws, from the key and K of its
-- entry. They cross 128-bit blocks, 1024-bit rows and chunks, and k1234_K8_u32 crosses a chunk
-- every 8 rows.
streamHashes :: IO ()
streamHashes = do
  cs <- loadCases "hashes.json"
  let streams = [c | c <- cs, hasField "file" c, text "type" c `notElem` ["Bool", "UInt128", "Float16", "Char"]]
  length streams @?= 8
  forM_ streams $ \c -> do
    let bytes = streamBytes (text "type" c) (number "n" c) (generator c)
    assertEqual (text "file" c) (number "bytes" c) (B.length bytes)
    assertEqual (text "file" c) (text "sha256" c) (sha256 bytes)

-- | The FNV-1a and SHA-256 of the long derived dumps: for each start a generator from the key and
-- K runs the listed fills in order, and the bytes of all starts are hashed in sequence.
dumpHashes :: IO ()
dumpHashes = do
  cs <- loadCases "hashes.json"
  let dumps = [c | c <- cs, not (hasField "file" c)]
  length dumps @?= 5
  forM_ dumps $ \c -> do
    let run (bs, _) st =
          let g0 = fromKey (caseKey c) (fromIntegral st) (fromIntegral (number "K" c))
              step (acc, g) (kind, n) = case kind of
                "fill_normal_f64" -> let (v, g') = fillNormal n g in (acc <> doubles v, g')
                "fill_normal_f32" -> let (v, g') = fillNormalFloat n g in (acc <> floats v, g')
                "fill_exponential_f64" -> let (v, g') = fillExponential n g in (acc <> doubles v, g')
                "fill_exponential_f32" -> let (v, g') = fillExponentialFloat n g in (acc <> floats v, g')
                _ -> error ("unknown draw " ++ kind)
              (b, g1) = foldl step (mempty, g0) (dumpDraws c)
           in (bs <> b, g1)
        (builder, final) = foldl run (mempty, seed 0) (numbers "starts" c)
        bytes = lazyBytes builder
        label = text "id" c
    assertEqual label (number "bytes" c) (B.length bytes)
    assertEqual label (hex (text "fnv1a" c)) (fnv1a bytes)
    when (hasField "sha256" c) $ assertEqual label (text "sha256" c) (sha256 bytes)
    when (hasField "end" c) $ assertEqual label (number "end" c) (fromIntegral (position final))

-- Positions -------------------------------------------------------------------------------

-- | A draw at any position equals the sequential fill, across block, row and chunk boundaries:
-- 2048 words at K = 8 cross two chunks.
randomAccess :: IO ()
randomAccess = forM_ [8, 32] $ \k -> do
  let g = fromKey key42 0 k
      w32 = fst (fillWord32 2048 g)
      w64 = fst (fillWord64 1024 g)
  forM_ [0 .. 2047] $ \i -> fst (nextWord32 (seek (32 * fromIntegral i) g)) @?= w32 U.! i
  forM_ [0 .. 1023] $ \i -> fst (nextWord64 (seek (64 * fromIntegral i) g)) @?= w64 U.! i

-- | A start of 2^63 - 1 is accepted, and 2^63 and 2^64 - 1 are rejected, and as a generator is a
-- value, a rejected start changes nothing. A 64-bit draw at 2^63 - 1 aligns to 2^63. A fill
-- reaches 2^64 only with 2^57 elements or more, which no vector holds, so the check in the fill
-- has no input that the tests can build.
positionBounds :: IO ()
positionBounds = do
  let top = 2 ^ (63 :: Int) :: Word64
      g = seek (top - 1) (fromKey key42 0 32)
  position g @?= top - 1
  fails (seek top g)
  fails (seek maxBound g)
  fails (fromKey key42 top 32)
  fails (fromKey key42 maxBound 32)
  position (snd (nextWord64 g)) @?= top + 64
  position (snd (fillWord64 1 g)) @?= top + 64
