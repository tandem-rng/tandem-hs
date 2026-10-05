-- | Tandem8x32, a noncryptographic pseudorandom number generator fast on CPUs and GPUs alike.
--
-- This module produces the stream of the
-- [specification](https://github.com/tandem-rng/spec), bit for bit. A 'Tandem' is a pure
-- value: its transport form (key, bit position, chunk length @K@) and a cache of the current
-- row. Every draw returns the value and the advanced generator.
--
-- > import System.Random.Tandem qualified as T
-- >
-- > let g0 = T.seed 42
-- >     (x, g1) = T.nextDouble g0
-- >     (zs, g2) = T.fillNormal 1000 g1   -- unboxed vector of standard normals
-- >     worker = T.split 7 g2             -- by index, from the key alone
-- >     (kids, g3) = T.fork 4 g2          -- from the current block, the parent moves on
--
-- Every draw aligns the position to its width, reads, and advances past it, as the
-- specification requires, so draws of mixed widths agree with the other ports. Bounded
-- integers, normals and exponentials follow Appendix A of the specification.
--
-- 'Tandem' is a 'RandomGen' and a 'SplitGen' of the @random@ package, so @random@'s stateful
-- adapters ('System.Random.Stateful.StateGenM', 'System.Random.Stateful.IOGenM',
-- 'System.Random.Stateful.AtomicGenM', 'System.Random.Stateful.STGenM') make it a
-- 'System.Random.Stateful.StatefulGen'.
module System.Random.Tandem
  ( -- * Generator
    Tandem
  , Quad (..)
  , seed
  , fromKey
  , defaultChunkLength
  , key
  , chunkLength
  , position
  , seek

    -- * Derived generators
  , split
  , fork
  , purpose

    -- * Scalar draws
  , nextWord32
  , nextWord64
  , nextDouble
  , nextFloat
  , nextBelow32
  , nextBelow64
  , nextNormal
  , nextNormalFloat
  , nextNormalPairFloat
  , nextExponential
  , nextExponentialFloat

    -- * Fills
    -- $fills
  , fillWord32
  , fillWord64
  , fillDouble
  , fillFloat
  , fillBelow32
  , fillBelow64
  , fillBelow
  , fillNormal
  , fillNormalFloat
  , fillExponential
  , fillExponentialFloat

    -- * Fills in place
  , fillWord32M
  , fillWord64M
  , fillDoubleM
  , fillFloatM
  , fillBelow32M
  , fillBelow64M
  , fillBelowM
  , fillNormalM
  , fillNormalFloatM
  , fillExponentialM
  , fillExponentialFloatM
  ) where

import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word32, Word64)

import System.Random.Tandem.Core (Quad (..))
import System.Random.Tandem.Derived
import System.Random.Tandem.Generator

-- $fills
-- A fill of @n@ elements returns the values of @n@ scalar draws and the generator after them.
-- It aligns the position once and then reads whole rows. A plain fill of 0 elements aligns the
-- position, as the specification defines, and so does a 'Double' normal fill. A bounded,
-- 'Float' normal or exponential fill of 0 elements leaves it as it is.

pureFill :: MU.Unbox a => (forall s. MU.MVector s a -> Tandem -> ST s Tandem) -> Int -> Tandem -> (U.Vector a, Tandem)
pureFill fill n g = runST $ do
  v <- MU.new n
  g' <- fill v g
  u <- U.unsafeFreeze v
  pure (u, g')
{-# INLINE pureFill #-}

-- | @n@ 32-bit draws.
fillWord32 :: Int -> Tandem -> (U.Vector Word32, Tandem)
fillWord32 = pureFill fillWord32M

-- | @n@ 64-bit draws.
fillWord64 :: Int -> Tandem -> (U.Vector Word64, Tandem)
fillWord64 = pureFill fillWord64M

-- | @n@ uniform 'Double's in @[0, 1)@.
fillDouble :: Int -> Tandem -> (U.Vector Double, Tandem)
fillDouble = pureFill fillDoubleM

-- | @n@ uniform 'Float's in @[0, 1)@.
fillFloat :: Int -> Tandem -> (U.Vector Float, Tandem)
fillFloat = pureFill fillFloatM

-- | @n@ uniform integers in @[0, r)@ from 32-bit draws, as 'fillBelow32M'.
fillBelow32 :: Word32 -> Int -> Tandem -> (U.Vector Word32, Tandem)
fillBelow32 r = pureFill (fillBelow32M r)

-- | @n@ uniform integers in @[0, r)@ from 64-bit draws, as 'fillBelow64M'.
fillBelow64 :: Word64 -> Int -> Tandem -> (U.Vector Word64, Tandem)
fillBelow64 r = pureFill (fillBelow64M r)

-- | @n@ uniform integers in @[0, r)@ with the draw width from the range, as 'fillBelowM'.
fillBelow :: Word64 -> Int -> Tandem -> (U.Vector Word64, Tandem)
fillBelow r = pureFill (fillBelowM r)

-- | @n@ standard normals, as 'fillNormalM'.
fillNormal :: Int -> Tandem -> (U.Vector Double, Tandem)
fillNormal = pureFill fillNormalM

-- | @n@ single-precision standard normals, as 'fillNormalFloatM'.
fillNormalFloat :: Int -> Tandem -> (U.Vector Float, Tandem)
fillNormalFloat = pureFill fillNormalFloatM

-- | @n@ standard exponentials, as 'fillExponentialM'.
fillExponential :: Int -> Tandem -> (U.Vector Double, Tandem)
fillExponential = pureFill fillExponentialM

-- | @n@ single-precision standard exponentials, as 'fillExponentialFloatM'.
fillExponentialFloat :: Int -> Tandem -> (U.Vector Float, Tandem)
fillExponentialFloat = pureFill fillExponentialFloatM
