module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (forM_, replicateM, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.ByteString qualified as B
import Data.List (mapAccumL, nub)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word32, Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import System.Random qualified as R
import System.Random.Stateful qualified as RS
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Fixtures
import System.Random.Tandem
import System.Random.Tandem.Core

main :: IO ()
main =
  defaultMain $
    testGroup
      "tandem"
      [specVectors, streamDumps, cache, crossFixtures, bitHashes, derivedFills, distributions, positions, randomGen]

-- | @n@ scalar draws.
draws :: Int -> (Tandem -> (a, Tandem)) -> Tandem -> ([a], Tandem)
draws n next g0 = let (g1, xs) = mapAccumL (\g _ -> let (x, g') = next g in (g', x)) g0 [1 .. n] in (xs, g1)

bits64 :: U.Vector Double -> [Word64]
bits64 = map castDoubleToWord64 . U.toList

bits32 :: U.Vector Float -> [Word32]
bits32 = map castFloatToWord32 . U.toList

-- | Seed @lo + hi 2^64@, tandem-c's @tandem_seed(lo, hi, 0)@.
seed2 :: Integer -> Integer -> Tandem
seed2 lo hi = seed (lo + hi * 2 ^ (64 :: Int))

fails :: a -> IO ()
fails x = do
  r <- try (evaluate x)
  case r of
    Left (_ :: ErrorCall) -> pure ()
    Right _ -> assertFailure "expected an error"

-- Specification ---------------------------------------------------------------------------

specVectors :: TestTree
specVectors =
  testGroup
    "specification vectors"
    [ testCase "T" $ forM_ stepVectors $ \(o, h, o', h') -> step o h @?= (o', h')
    , testCase "F" $ forM_ fVectors $ \(c, o, h) -> keyedF vectorKey c domainStream auxStream @?= (o, h)
    , testCase "stream words" $ do
        let g = fromKey vectorKey 0 32
            (ws, _) = fillWord32 36 g
        forM_ streamWords $ \(i, want) -> do
          take 4 (drop i (U.toList ws)) @?= want
          fst (draws 4 nextWord32 (seek (32 * fromIntegral i) g)) @?= want
    , testCase "blocks" $ do
        let quadList (Quad a b c d) = [a, b, c, d]
        quadList (block vectorKey 0 0) @?= snd (streamWords !! 0)
        quadList (block vectorKey 1 0) @?= snd (streamWords !! 1)
        quadList (block vectorKey 0 1) @?= snd (streamWords !! 2)
    , testCase "draws from position 0" $ do
        let g = fromKey vectorKey 0 32
        forM_ vectorF64 $ \(i, want) -> bits64 (fst (fillDouble 17 g)) !! i @?= want
        forM_ vectorF32 $ \(i, want) -> bits32 (fst (fillFloat 3 g)) !! i @?= want
    , testCase "derived keys" $ do
        let g = fromKey vectorKey 0 32
        key (split 0 g) @?= splitChild0
        key (split 1 g) @?= splitChild1
        map key (take 1 (fst (fork 1 g))) @?= [forkChild0]
        key (purpose 7 g) @?= purpose7
    , testCase "seed whitening" $ do
        let g = seed seedValue
        key g @?= seedKey
        forM_ seedF64 $ \(i, want) -> bits64 (fst (fillDouble 17 g)) !! i @?= want
        forM_ seedU32 $ \(i, want) -> fst (fillWord32 (i + 1) g) U.! i @?= want
    ]

-- Stream dumps ----------------------------------------------------------------------------

-- | The dump as little-endian words of @n@ bytes.
leWords :: Int -> B.ByteString -> [Word64]
leWords n bs
  | B.null bs = []
  | otherwise =
      foldr (\b acc -> acc `shiftL` 8 + fromIntegral b) 0 (B.unpack (B.take n bs)) : leWords n (B.drop n bs)

-- | A dump against the fill, against as many scalar draws, and against the same fill cut into
-- pieces in place, with equal end positions.
dump
  :: (Eq b, Show b, MU.Unbox a)
  => String
  -> Tandem
  -> Int
  -> (Int -> Tandem -> (U.Vector a, Tandem))
  -> (forall s. MU.MVector s a -> Tandem -> ST s Tandem)
  -> (Tandem -> (a, Tandem))
  -> (a -> b)
  -> (Word64 -> b)
  -> TestTree
dump file g bytes fill fillM next view raw = testCase file $ do
  want <- map raw . leWords bytes <$> B.readFile ("tests/data/" ++ file)
  let n = length want
      (v, g1) = fill n g
      (xs, g2) = draws n next g
      (cut, g3) = runST $ do
        m <- MU.new n
        let go _ [] h = pure h
            go i (c : cs) h = fillM (MU.slice i c m) h >>= go (i + c) cs
        h <- go 0 (pieces n) g
        u <- U.freeze m
        pure (u, h)
  map view (U.toList v) @?= want
  map view xs @?= want
  map view (U.toList cut) @?= want
  position g1 @?= position g2
  position g3 @?= position g2
  where
    pieces n = let cs = takeWhile (< n) (scanl1 (+) [1, 37, 500, 3, 2000]) in zipWith (-) (cs ++ [n]) (0 : cs)

streamDumps :: TestTree
streamDumps =
  testGroup
    "stream dumps"
    [ dump "k1234_K32_u32.bin" k32 4 fillWord32 fillWord32M nextWord32 id fromIntegral
    , dump "k1234_K8_u32.bin" (fromKey vectorKey 0 8) 4 fillWord32 fillWord32M nextWord32 id fromIntegral
    , dump "k1234_K32_u64.bin" k32 8 fillWord64 fillWord64M nextWord64 id id
    , dump "seed42_K32_f32.bin" s42 4 fillFloat fillFloatM nextFloat castFloatToWord32 fromIntegral
    , dump "seed42_K32_f64.bin" s42 8 fillDouble fillDoubleM nextDouble castDoubleToWord64 id
    , dump "seed42_K32_c32.bin" s42 4 fillFloat fillFloatM nextFloat castFloatToWord32 fromIntegral
    , dump "seed42_K32_c64.bin" s42 8 fillDouble fillDoubleM nextDouble castDoubleToWord64 id
    , testCase "seed42_K32_u8.bin" $ do
        want <- B.readFile "tests/data/seed42_K32_u8.bin"
        fst (draws (B.length want) R.genWord8 s42) @?= B.unpack want
    ]
  where
    k32 = fromKey vectorKey 0 32
    s42 = seed 42

-- | The 32-bit word at stream bit @p@ by the definition of section 4.
wordRef :: Quad -> Word32 -> Word64 -> Word32
wordRef k kk p =
  let r = p `shiftR` 10
      c = 8 * (r `div` fromIntegral kk) + ((p `shiftR` 7) .&. 7)
      Quad a b d e = block k c (fromIntegral (r `mod` fromIntegral kk))
   in [a, b, d, e] !! fromIntegral ((p `shiftR` 5) .&. 3)

-- | Fills and draws that jump around the cache, at chunk lengths from 1 to 65536, against the
-- definition.
cache :: TestTree
cache = testCase "random access at every chunk length" $
  forM_ [1, 2, 8, 32, 65536] $ \kk ->
    forM_ [(0, 1100), (3 * 1024 + 7, 40), (5 * 1024 * 31 - 64, 900), (2 ^ (40 :: Int) + 96, 300), (32, 5)] $ \(p0, n) -> do
      let g = seek 999999 (snd (fillWord32 3 (fromKey vectorKey 70000 kk)))
          (v, g') = fillWord32 n (seek p0 g)
          p = (p0 + 31) .&. complement31
      U.toList v @?= [wordRef vectorKey kk (p + 32 * fromIntegral i) | i <- [0 .. n - 1]]
      fst (nextWord32 (seek p g')) @?= wordRef vectorKey kk p
  where
    complement31 = maxBound - 31

-- Cross-check fixtures --------------------------------------------------------------------

crossFixtures :: TestTree
crossFixtures =
  testGroup
    "cross fixtures"
    [ testCase "scalar bounded draws" $ do
        forM_ below32 $ \(n, want, end) -> do
          let (xs, g) = draws 64 (nextBelow32 n) start
          (xs, position g) @?= (want, end)
        forM_ below64 $ \(n, want, end) -> do
          let (xs, g) = draws 64 (nextBelow64 n) start
          (xs, position g) @?= (want, end)
    , testCase "bounded fills" $ do
        rejected <- fmap or . sequence $
          [ do
              let (v, g) = fillBelow32 n 64 (seek p (seed 42))
              (U.toList v, position g) @?= (want, end)
              pure (want /= fst (draws 64 (nextBelow32 n) (seek p (seed 42))))
          | (p, n, want, end) <- crossFillBelow32
          ]
        assertBool "some fill takes the fallback" rejected
        forM_ crossFillBelow64 $ \(p, n, want, end) -> do
          let (v, g) = fillBelow64 n 64 (seek p (seed 42))
          (U.toList v, position g) @?= (want, end)
    , testCase "tandem-cuda bounded fills" $ do
        key (seed 42) @?= cudaKey
        forM_ cudaBelow32 $ \(n, _, want) -> U.toList (fst (fillBelow32 n 64 (fromKey cudaKey 0 32))) @?= want
        forM_ cudaBelow64 $ \(n, _, want) -> U.toList (fst (fillBelow64 n 64 (fromKey cudaKey 0 32))) @?= want
        forM_ cudaBelow32At $ \(p, n, _, want) -> U.toList (fst (fillBelow32 n 64 (fromKey cudaKey p 32))) @?= want
        forM_ cudaBelow64At $ \(p, n, _, want) -> U.toList (fst (fillBelow64 n 64 (fromKey cudaKey p 32))) @?= want
    , testCase "normal pairs" $ do
        let (ps, g) = draws 64 nextNormalPair start
            (fs, h) = draws 64 nextNormalPairFloat start
        (concatMap (\(c, s) -> map castDoubleToWord64 [c, s]) ps, position g) @?= normalPairs
        (concatMap (\(c, s) -> map castFloatToWord32 [c, s]) fs, position h) @?= normalPairsFloat
    , testCase "tandem-cuda normal and exponential fills" $ do
        let at p = fromKey cudaKey p 32
        forM_ cudaNormal64 $ \(p, n, want) -> bits64 (fst (fillNormal n (at p))) @?= want
        forM_ cudaNormal32 $ \(p, n, want) -> bits32 (fst (fillNormalFloat n (at p))) @?= want
        forM_ cudaExponential64 $ \(p, n, want) -> bits64 (fst (fillExponential n (at p))) @?= want
        forM_ cudaExponential32 $ \(p, n, want) -> bits32 (fst (fillExponentialFloat n (at p))) @?= want
    , testCase "exponentials" $ do
        forM_ exponentials $ \(p, want, end) -> do
          let (v, g) = fillExponential 64 (seek p (seed 42))
              (xs, h) = draws 64 nextExponential (seek p (seed 42))
          (bits64 v, position g) @?= (want, end)
          (map castDoubleToWord64 xs, position h) @?= (want, end)
        forM_ exponentialsFloat $ \(p, want, end) -> do
          let (v, g) = fillExponentialFloat 64 (seek p (seed 42))
              (xs, h) = draws 64 nextExponentialFloat (seek p (seed 42))
          (bits32 v, position g) @?= (want, end)
          (map castFloatToWord32 xs, position h) @?= (want, end)
    ]
  where
    -- tandem-c's fixtures start after one Bool draw.
    start = seek 1 (seed 42)

-- Bit hashes ------------------------------------------------------------------------------

fnv :: Word64 -> Int -> Word64 -> Word64
fnv h bytes x = foldl' (\a i -> (a `xor` ((x `shiftR` (8 * i)) .&. 0xff)) * 0x100000001b3) h [0 .. bytes - 1]

hashDoubles :: Word64 -> U.Vector Double -> Word64
hashDoubles = U.foldl' (\h x -> fnv h 8 (castDoubleToWord64 x))

hashFloats :: Word64 -> U.Vector Float -> Word64
hashFloats = U.foldl' (\h x -> fnv h 4 (fromIntegral (castFloatToWord32 x)))

-- | The hashes of tandem-c's tests/test_normal_bits.c and tests/test_exponential_bits.c.
bitHashes :: TestTree
bitHashes =
  testGroup
    "bit hashes"
    [ testCase "normals" $ hashOf (2 * 1000000 - 1) fillNormal fillNormalFloat @?= 0x9414e1315e2653be
    , testCase "exponentials" $ hashOf 1000000 fillExponential fillExponentialFloat @?= 0x47f8f98297d94ee2
    ]
  where
    hashOf n f64 f32 =
      foldl'
        ( \h p ->
            let (d, g) = f64 n (seek p (seed2 2026 7))
                (f, _) = f32 n g
             in hashFloats (hashDoubles h d) f
        )
        0xcbf29ce484222325
        [0, 1, 77, 12345, 2 ^ (30 :: Int)]

-- Derived fills ---------------------------------------------------------------------------

derivedFills :: TestTree
derivedFills =
  testGroup
    "derived fills"
    [ testCase "bounded fills cut anywhere equal the whole" $
        forM_ [1, 12345, 100000] $ \p -> forM_ [1, 7, 300, 1000, 2999] $ \c -> do
          let g = seek p (seed2 5 6)
          cutEquals c (fillBelow32M 0xc0000001) g
          cutEquals c (fillBelow64M 0xc000000000000001) g
    , testCase "fills longer than one tandem-c call" $ do
        let n = 2 ^ (20 :: Int) + 3
            k = n - 7
            g = seek 77 (seed 8)
            from w = seek ((77 + w - 1) `div` w * w + w * fromIntegral k) g
            check :: (U.Unbox a, Eq a, Show a) => (Int -> Tandem -> (U.Vector a, Tandem)) -> Word64 -> IO ()
            check fill w = do
              let (v, h) = fill n g
                  (u, h') = fill 7 (from w)
              (U.drop k v, position h) @?= (u, position h')
        check fillWord32 32
        check (fillBelow32 0xc0000001) 32
        check (fillBelow64 0xc000000000000001) 64
        check fillNormal 64
        check fillNormalFloat 32
        check fillExponential 64
    , testCase "bounded fills of one key share no fallback" $ do
        let g = seed2 5 6
            x = fst (fillBelow32 0xc0000001 300 g)
            y = fst (fillBelow32 0xc0000001 299 (seek 32 g))
        U.tail x @?= y
    , testCase "bounded fill widths from the range" $ do
        let g = seek 3 (seed 9)
            widen = U.map fromIntegral
        fst (fillBelow 1000 100 g) @?= widen (fst (fillBelow32 1000 100 g))
        fst (fillBelow (2 ^ (32 :: Int)) 100 g) @?= widen (fst (fillWord32 100 g))
        fst (fillBelow (2 ^ (40 :: Int)) 100 g) @?= fst (fillBelow64 (2 ^ (40 :: Int)) 100 g)
        position (snd (fillBelow 1000 5 g)) @?= 32 * 6
    , testCase "range 0 gives 0 after one draw" $ do
        let g = seed2 1 2
            (a, g1) = nextBelow32 0 g
            (b, g2) = nextBelow64 0 g1
        (a, position g1, b, position g2) @?= (0, 32, 0, 128)
    , testCase "normal fills are the flattened pairs" $
        forM_ [0 .. 9] $ \n -> forM_ [0, 1, 77] $ \p -> forM_ [n, n + 8192] $ \m -> do
          let g = seek p (seed 3)
              (ps, h) = draws ((m + 1) `div` 2) nextNormalPair g
              (fs, hf) = draws ((m + 1) `div` 2) nextNormalPairFloat g
              (v, g') = fillNormal m g
              (vf, gf) = fillNormalFloat m g
          U.toList v @?= take m (concatMap (\(c, s) -> [c, s]) ps)
          U.toList vf @?= take m (concatMap (\(c, s) -> [c, s]) fs)
          (position g', position gf) @?= (position h, position hf)
          when (m > 0) $ do
            U.head v @?= fst (nextNormal g)
            U.head vf @?= fst (nextNormalFloat g)
    , testCase "exponential fills are the scalar draws" $
        forM_ [0, 1, 5, 4097] $ \n -> do
          let g = seek 77 (seed 3)
              (xs, h) = draws n nextExponential g
              (fs, hf) = draws n nextExponentialFloat g
              (v, g') = fillExponential n g
              (vf, gf) = fillExponentialFloat n g
          (U.toList v, position g') @?= (xs, position h)
          (U.toList vf, position gf) @?= (fs, position hf)
    ]
  where
    cutEquals
      :: (MU.Unbox a, Eq a, Show a)
      => Int -> (forall s. MU.MVector s a -> Tandem -> ST s Tandem) -> Tandem -> IO ()
    cutEquals c fillM g = do
      let run cuts = runST $ do
            m <- MU.new 3000
            let go _ [] h = pure h
                go i (k : ks) h = fillM (MU.slice i k m) h >>= go (i + k) ks
            h <- go 0 cuts g
            u <- U.freeze m
            pure (U.toList u, position h)
      run [c, 3000 - c] @?= run [3000]

-- Distributions ---------------------------------------------------------------------------

-- | Raw moments 1 to 4 of 10^7 draws, each within 5 standard errors of the exact value, and the
-- Kolmogorov-Smirnov distance under its 0.1% critical value.
distributions :: TestTree
distributions =
  testGroup
    "distributions"
    [ check "normal" normalMoments normalCdf (fst (fillNormal n (seed 4)))
    , check "normal Float" normalMoments normalCdf (toDouble (fst (fillNormalFloat n (seed 5))))
    , check "exponential" expMoments expCdf (fst (fillExponential n (seed 6)))
    , check "exponential Float" expMoments expCdf (toDouble (fst (fillExponentialFloat n (seed 7))))
    ]
  where
    n = 10000000
    toDouble = U.map realToFrac
    normalMoments = [0, 1, 0, 3, 0, 15, 0, 105]
    expMoments = [1, 2, 6, 24, 120, 720, 5040, 40320]
    expCdf x = 1 - exp (negate x)
    check name exact cdf v = testCase name $ do
      let size = fromIntegral (U.length v)
      forM_ [1 .. 4] $ \k -> do
        let m = U.sum (U.map (^ k) v) / size
            mu = exact !! (k - 1)
            se = sqrt ((exact !! (2 * k - 1) - mu * mu) / size)
        assertBool ("moment " ++ show k ++ ": " ++ show m) (abs (m - mu) < 5 * se)
      let d = ksDistance cdf v
      assertBool ("KS distance " ++ show d) (d < 1.95 / sqrt size)

-- | The KS distance from counts in 2^22 equal bins of the CDF's range. Binning changes it by at
-- most a bin width plus the fullest bin's share, both far below the critical value at 10^7.
ksDistance :: (Double -> Double) -> U.Vector Double -> Double
ksDistance cdf v = U.maximum (U.imap gap (U.scanl1' (+) counts))
  where
    bins = 2 ^ (22 :: Int)
    size = fromIntegral (U.length v) :: Double
    counts = runST $ do
      c <- MU.replicate bins (0 :: Int)
      U.forM_ v $ \x -> MU.unsafeModify c (+ 1) (min (bins - 1) (floor (cdf x * fromIntegral bins)))
      U.unsafeFreeze c
    gap i c = abs (fromIntegral c / size - fromIntegral (i + 1) / fromIntegral bins)

-- | The standard normal CDF through Numerical Recipes' erfc, good to 1.2e-7 relative.
normalCdf :: Double -> Double
normalCdf x = 0.5 * erfc (negate x / sqrt 2)
  where
    erfc z
      | z < 0 = 2 - erfc (negate z)
      | otherwise =
          let t = 1 / (1 + 0.5 * z)
           in t * exp (negate (z * z) - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418
                + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587
                + t * (-0.82215223 + t * 0.17087277)))))))))

-- Positions -------------------------------------------------------------------------------

positions :: TestTree
positions =
  testGroup
    "positions"
    [ testCase "empty fills" $
        forM_ [1, 5, 33, 65, 1001] $ \p -> do
          let g = seek p (seed2 1 2)
              at = position . snd
          [ at (fillBelow32 10 0 g), at (fillBelow64 10 0 g), at (fillBelow 10 0 g)
            , at (fillNormal 0 g), at (fillNormalFloat 0 g)
            , at (fillExponential 0 g), at (fillExponentialFloat 0 g) ]
            @?= replicate 7 p
          position (snd (fillWord64 0 g)) @?= (p + 63) `div` 64 * 64
          position (snd (fillFloat 0 g)) @?= (p + 31) `div` 32 * 32
    , testCase "draws align" $ do
        let g = seek 33 (seed 1)
        position (snd (nextWord64 g)) @?= 128
        position (snd (nextFloat g)) @?= 96
        position (snd (fillWord32 3 g)) @?= 160
    , testCase "split and purpose keep the parent" $ do
        let g = seek 1234 (seed 1)
        position (split 5 g) @?= 0
        position (purpose 5 g) @?= 0
        chunkLength (split 5 (fromKey vectorKey 0 8)) @?= 8
        key (split 5 g) @?= key (split 5 (seek 0 g))
    , testCase "fork advances once per batch" $ do
        let g = seek 300 (seed 1)
            (kids, g') = fork 5 g
        length kids @?= 5
        length (nub (map key kids)) @?= 5
        position g' @?= 384
        position (snd (fork 0 g)) @?= 384
        position (snd (fork 1 (seek 256 g))) @?= 384
        map key (fst (fork 3 g)) @?= map key (fst (fork 3 (seek 260 g)))
    , testCase "bounds" $ do
        fails (fromKey vectorKey (2 ^ (63 :: Int)) 32)
        fails (fromKey vectorKey 0 3)
        fails (fromKey vectorKey 0 131072)
        fails (seek (2 ^ (63 :: Int)) (seed 1))
        fails (seed (-1))
        fails (seed (2 ^ (128 :: Int)))
        fails (fork (2 ^ (33 :: Int) + 1) (seed 1))
    , testCase "transport form" $ do
        let g = snd (fillWord32 5 (seed 8))
        fromKey (key g) (position g) (chunkLength g) @?= g
        fst (fillDouble 50 (fromKey (key g) (position g) (chunkLength g))) @?= fst (fillDouble 50 g)
    ]

-- random ----------------------------------------------------------------------------------

randomGen :: TestTree
randomGen =
  testGroup
    "random"
    [ testCase "genWord32 and genWord64 are the draws" $ do
        let g = seek 7 (seed 11)
        fst (draws 100 R.genWord32 g) @?= fst (draws 100 nextWord32 g)
        fst (draws 100 R.genWord64 g) @?= fst (draws 100 nextWord64 g)
    , testCase "genWord8 and genWord16 draw their widths" $ do
        let g = seed 11
            w = fst (nextWord32 g)
            byte i = fromIntegral (w `shiftR` (8 * i)) :: Word8
        fst (draws 4 R.genWord8 g) @?= map byte [0 .. 3]
        position (snd (R.genWord16 (seek 1 g))) @?= 32
    , testCase "bounded draws are Lemire's" $ do
        let g = seed 12
        forM_ [0, 1, 999, 0xc0000000, maxBound - 1] $ \m ->
          fst (draws 50 (R.genWord32R m) g) @?= fst (draws 50 (nextBelow32 (m + 1)) g)
        fst (draws 50 (R.genWord32R maxBound) g) @?= fst (draws 50 nextWord32 g)
        forM_ [0, 999, 0xfffffffe] $ \m -> do
          let (xs, h) = draws 50 (R.genWord64R m) g
              (ys, h') = draws 50 (R.genWord32R (fromIntegral m)) g
          (xs, position h) @?= (map fromIntegral ys, position h')
        fst (draws 50 (R.genWord64R 0xffffffff) g) @?= map fromIntegral (fst (draws 50 nextWord32 g))
        forM_ [2 ^ (32 :: Int), 0xc000000000000000] $ \m ->
          fst (draws 50 (R.genWord64R m) g) @?= fst (draws 50 (nextBelow64 (m + 1)) g)
        fst (draws 50 (R.genWord64R maxBound) g) @?= fst (draws 50 nextWord64 g)
    , testCase "uniformR stays in range" $ do
        let (xs, _) = draws 2000 (R.uniformR (-3 :: Int, 4)) (seed 13)
            (ys, _) = draws 2000 (R.uniformR (2.5 :: Double, 3)) (seed 13)
        assertBool "Int" (all (\x -> x >= -3 && x <= 4) xs && length (nub xs) == 8)
        assertBool "Double" (all (\y -> y >= 2.5 && y <= 3) ys)
    , testCase "splitGen is fork 1" $ do
        let g = seek 500 (seed 14)
            (g', c) = R.splitGen g
            (kids, h) = fork 1 g
        (g', [c]) @?= (h, kids)
        assertBool "streams differ" (fst (fillWord64 8 g') /= fst (fillWord64 8 c))
        assertBool "repeated splits differ" (key (snd (R.splitGen g')) /= key c)
    , testCase "draws are pure" $ do
        let g = seed 15
        fst (R.uniform g :: (Word64, Tandem)) @?= fst (R.uniform g)
    , testCase "stateful adapters give the pure draws" $ do
        let g = seed 16
            want = fst (draws 40 nextWord64 g)
        fst (RS.runStateGen g (replicateM 40 . RS.uniformWord64)) @?= want
        fst (RS.runSTGen g (replicateM 40 . RS.uniformWord64)) @?= want
        io <- RS.newIOGenM g
        (@?= want) =<< replicateM 40 (RS.uniformWord64 io)
        at <- RS.newAtomicGenM g
        (@?= want) =<< replicateM 40 (RS.uniformWord64 at)
        r <- RS.uniformRM (1 :: Word32, 6) io
        assertBool "die in range" (r >= 1 && r <= 6)
    ]
