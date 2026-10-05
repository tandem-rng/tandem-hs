{-# LANGUAGE RankNTypes #-}

-- | Fills of 2^22 values, short fills that stay in Haskell, and loops of 2^20 scalar draws, against
-- random's StdGen (SplitMix) and mwc-random. Prints Markdown rows: fills in GiB/s of output,
-- scalar draws in ns per draw.
module Main (main) where

import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word32, Word64)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Random qualified as R
import System.Random.MWC qualified as MWC
import System.Random.MWC.Distributions qualified as MWCD
import System.Random.Stateful qualified as RS
import System.Random.Tandem qualified as T
import Test.Tasty (mkTimeout)
import Test.Tasty.Bench (Benchmarkable, RelStDev (..), measureCpuTime, whnf, whnfIO)
import Text.Printf (printf)

n, m :: Int
n = 2 ^ (22 :: Int)
m = 2 ^ (20 :: Int)

-- | The sum of @k@ scalar draws.
loop :: Num a => (g -> (a, g)) -> Int -> g -> a
loop next k = go k 0
  where
    go 0 !acc _ = acc
    go i !acc g = let !(x, g') = next g in go (i - 1) (acc + x) g'

loopIO :: Num a => IO a -> Int -> IO a
loopIO next k = go k 0
  where
    go 0 !acc = pure acc
    go i !acc = next >>= \x -> go (i - 1) (acc + x)

-- | 2^13 fills of 2^9 values into one vector, below the size where fills call tandem-c.
short :: forall a. U.Unbox a => (forall s. MU.MVector s a -> T.Tandem -> ST s T.Tandem) -> T.Tandem -> Word64
short f g0 = runST (MU.unsafeNew 512 >>= \v -> fills v (8192 :: Int) g0)
  where
    fills :: MU.MVector s a -> Int -> T.Tandem -> ST s Word64
    fills _ 0 g = pure (T.position g)
    fills v i g = f v g >>= fills v (i - 1)

-- | A table row: its label, how to turn seconds into the printed figure, and the Tandem, StdGen
-- and mwc cases.
data Row = Row String (Double -> Double) [Maybe Benchmarkable]

-- | GiB/s of a fill writing @values@ of @bytes@ each.
rate :: Int -> Int -> Double -> Double
rate values bytes t = fromIntegral (values * bytes) / t / 2 ^ (30 :: Int)

-- | ns per draw of a loop of @m@ draws.
perDraw :: Double -> Double
perDraw t = t / fromIntegral m * 1e9

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  gen <- MWC.create
  let t = T.seed 42
      s = R.mkStdGen 42
      fill f = Just (whnf (\g -> fst (f n g)) t)
      unfold :: U.Unbox a => (R.StdGen -> (a, R.StdGen)) -> Maybe Benchmarkable
      unfold f = Just (whnf (U.unfoldrExactN n f) s)
      io = Just . whnfIO
      long label bytes cases = Row label (rate n bytes) cases
      shortRow
        :: U.Unbox a => String -> Int -> (forall s. MU.MVector s a -> T.Tandem -> ST s T.Tandem) -> Row
      shortRow label bytes f = Row (label ++ ", 2^9 values") (rate (512 * 8192) bytes) [Just (whnf (short f) t)]
      scalar label cases = Row label perDraw cases
      fills =
        [ long "fill `Word32`" 4
            [fill T.fillWord32, unfold R.genWord32, io (MWC.uniformVector gen n :: IO (U.Vector Word32))]
        , long "fill `Double`" 8
            [fill T.fillDouble, unfold (R.uniformR (0, 1 :: Double)), io (U.replicateM n (RS.uniformDouble01M gen))]
        , long "fill `Float`" 4
            [fill T.fillFloat, unfold (R.uniformR (0, 1 :: Float)), io (U.replicateM n (RS.uniformFloat01M gen))]
        , long "fill bounded, range 1000" 4
            [ fill (T.fillBelow32 1000)
            , unfold (R.uniformR (0, 999 :: Word32))
            , io (U.replicateM n (MWC.uniformR (0, 999 :: Word32) gen))
            ]
        , long "fill normal `Double`" 8 [fill T.fillNormal, Nothing, io (U.replicateM n (MWCD.standard gen))]
        , long "fill exponential `Double`" 8
            [fill T.fillExponential, Nothing, io (U.replicateM n (MWCD.exponential 1 gen))]
        , shortRow "fill `Word32`" 4 T.fillWord32M
        , shortRow "fill `Double`" 8 T.fillDoubleM
        , shortRow "fill `Float`" 4 T.fillFloatM
        , shortRow "fill normal `Double`" 8 T.fillNormalM
        ]
      scalars =
        [ scalar "scalar `Word64`"
            [ Just (whnf (loop T.nextWord64 m) t)
            , Just (whnf (loop R.genWord64 m) s)
            , io (loopIO (MWC.uniform gen :: IO Word64) m)
            ]
        , scalar "scalar `Double`"
            [ Just (whnf (loop T.nextDouble m) t)
            , Just (whnf (loop (R.uniformR (0, 1 :: Double)) m) s)
            , io (loopIO (RS.uniformDouble01M gen) m)
            ]
        , scalar "scalar normal `Double`"
            [Just (whnf (loop T.nextNormal m) t), Nothing, io (loopIO (MWCD.standard gen) m)]
        ]
  putStrLn "Fills, GiB/s of output\n\n| | Tandem | StdGen | mwc |\n|---|---|---|---|"
  mapM_ printRow fills
  putStrLn "\nScalar draws, ns per draw\n\n| | Tandem | StdGen | mwc |\n|---|---|---|---|"
  mapM_ printRow scalars
  where
    printRow (Row label figure cases) = do
      cells <- mapM (maybe (pure "") (fmap (cell . figure) . measureCpuTime (mkTimeout 3000000) (RelStDev 0.05))) cases
      putStrLn ("| " ++ label ++ concatMap (" | " ++) (take 3 (cells ++ repeat "")) ++ " |")
    cell x
      | x >= 10 = printf "%.1f" x
      | otherwise = printf "%.2f" x
