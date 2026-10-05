{-# LANGUAGE RankNTypes #-}

-- | Fills of 2^22 values, short fills that stay in Haskell, and loops of 2^20 scalar draws, against
-- random's StdGen (SplitMix) and mwc-random.
module Main (main) where

import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word32, Word64)
import System.Random qualified as R
import System.Random.Stateful qualified as RS
import System.Random.MWC qualified as MWC
import System.Random.MWC.Distributions qualified as MWCD
import System.Random.Tandem qualified as T
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, whnf, whnfIO)

n, m :: Int
n = 2 ^ (22 :: Int)
m = 2 ^ (20 :: Int)

-- | The sum of @k@ scalar draws.
loop :: Num a => (g -> (a, g)) -> Int -> g -> a
loop next k = go k 0
  where
    go 0 !acc _ = acc
    go i !acc g = let !(x, g') = next g in go (i - 1) (acc + x) g'

-- | 2^13 fills of 2^9 values into one vector, below the size where fills call tandem-c.
short :: forall a. U.Unbox a => (forall s. MU.MVector s a -> T.Tandem -> ST s T.Tandem) -> T.Tandem -> Word64
short f g0 = runST (MU.unsafeNew 512 >>= \v -> fills v (8192 :: Int) g0)
  where
    fills :: MU.MVector s a -> Int -> T.Tandem -> ST s Word64
    fills _ 0 g = pure (T.position g)
    fills v i g = f v g >>= fills v (i - 1)

loopIO :: Num a => IO a -> Int -> IO a
loopIO next k = go k 0
  where
    go 0 !acc = pure acc
    go i !acc = next >>= \x -> go (i - 1) (acc + x)

main :: IO ()
main = do
  gen <- MWC.create
  let t = T.seed 42
      s = R.mkStdGen 42
      fill f = whnf (\g -> fst (f n g)) t
      unfold :: U.Unbox a => (R.StdGen -> (a, R.StdGen)) -> Benchmark
      unfold f = bench "StdGen" (whnf (U.unfoldrExactN n f) s)
  defaultMain
    [ bgroup
          "fill 2^9, 2^13 times"
          [ bench "u32" (whnf (short T.fillWord32M) t)
          , bench "f64" (whnf (short T.fillDoubleM) t)
          , bench "f32" (whnf (short T.fillFloatM) t)
          , bench "normal f64" (whnf (short T.fillNormalM) t)
          ]
    , bgroup
          "fill 2^22"
          [ bgroup
              "u32"
              [ bench "Tandem" (fill T.fillWord32)
              , unfold R.genWord32
              , bench "mwc" (whnfIO (MWC.uniformVector gen n :: IO (U.Vector Word32)))
              ]
          , bgroup
              "f64"
              [ bench "Tandem" (fill T.fillDouble)
              , unfold (R.uniformR (0, 1 :: Double))
              , bench "mwc" (whnfIO (U.replicateM n (RS.uniformDouble01M gen)))
              ]
          , bgroup
              "f32"
              [ bench "Tandem" (fill T.fillFloat)
              , unfold (R.uniformR (0, 1 :: Float))
              , bench "mwc" (whnfIO (U.replicateM n (RS.uniformFloat01M gen)))
              ]
          , bgroup
              "below 1000"
              [ bench "Tandem" (fill (T.fillBelow32 1000))
              , unfold (R.uniformR (0, 999 :: Word32))
              , bench "mwc" (whnfIO (U.replicateM n (MWC.uniformR (0, 999 :: Word32) gen)))
              ]
          , bgroup
              "normal f64"
              [ bench "Tandem" (fill T.fillNormal)
              , bench "mwc" (whnfIO (U.replicateM n (MWCD.standard gen)))
              ]
          , bgroup
              "exponential f64"
              [ bench "Tandem" (fill T.fillExponential)
              , bench "mwc" (whnfIO (U.replicateM n (MWCD.exponential 1 gen)))
              ]
          ]
    , bgroup
          "scalar 2^20"
          [ bgroup
              "u64"
              [ bench "Tandem" (whnf (loop T.nextWord64 m) t)
              , bench "StdGen" (whnf (loop R.genWord64 m) s)
              , bench "mwc" (whnfIO (loopIO (MWC.uniform gen :: IO Word64) m))
              ]
          , bgroup
              "f64"
              [ bench "Tandem" (whnf (loop T.nextDouble m) t)
              , bench "StdGen" (whnf (loop (R.uniformR (0, 1 :: Double)) m) s)
              , bench "mwc" (whnfIO (loopIO (RS.uniformDouble01M gen) m))
              ]
          , bgroup
              "normal f64"
              [ bench "Tandem" (whnf (loop T.nextNormal m) t)
              , bench "mwc" (whnfIO (loopIO (MWCD.standard gen) m))
              ]
          ]
    ]
