-- | Bounded fills, normals and exponentials, as Appendix A of the specification describes them,
-- so that every port returns the same values.
module System.Random.Tandem.Derived
  ( nextNormal
  , nextNormalFloat
  , nextNormalPair
  , nextNormalPairFloat
  , nextExponential
  , nextExponentialFloat
  , fillBelow32M
  , fillBelow64M
  , fillBelowM
  , fillNormalM
  , fillNormalFloatM
  , fillExponentialM
  , fillExponentialFloatM
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Bits (shiftR)
import Data.Vector.Primitive.Mutable qualified as P
import Data.Vector.Unboxed.Base qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word32, Word64)

import System.Random.Tandem.Generator
import System.Random.Tandem.Math
import System.Random.Tandem.Native

-- Bounded integers ------------------------------------------------------------------------

-- | Reserved purposes of the fallback generators of the bounded fills.
purposeBelow32, purposeBelow64 :: Word64
purposeBelow32 = 0x424c573332
purposeBelow64 = 0x424c573634

-- | Map the raw draws of a fill in place. Element @i@ has the global draw index @first + i@, and a
-- rejected draw retries on the draws of @split (first + i)@ of the fallback generator.
bound
  :: (MU.Unbox w, Integral w)
  => (w -> w -> (w, w))
  -> (Tandem -> (w, Tandem))
  -> Tandem
  -> Word64
  -> w
  -> MU.MVector s w
  -> ST s ()
bound mul next fallback first n v = go 0
  where
    t = negate n `rem` n
    go i
      | i == MU.length v = pure ()
      | otherwise = do
          x <- MU.unsafeRead v i
          let (h, l) = mul x n
          MU.unsafeWrite v i $!
            if l < n && l < t
              then fst (lemire mul next n (split (first + fromIntegral i) fallback))
              else h
          go (i + 1)
{-# INLINE bound #-}

-- | The fallback generator of a bounded fill: @purpose p@ of the fill's key at position 0.
fallbackOf :: Word64 -> Tandem -> Tandem
fallbackOf p g = purpose p (restart (key g) (chunkLength g))

-- | Fill with uniform integers in @[0, n)@. Element @i@ takes draw @i@ of the 32-bit fill, so the
-- fill consumes exactly as many draws as it has elements, and an empty fill leaves the position
-- as it is. A rejected draw retries on @split g@ of @purpose 0x424c573332@ of the fill's key at
-- position 0, where @g@ is the draw's index in the stream, so a fill cut at any element equals
-- the whole fill. Without rejections the fill equals the scalar 'nextBelow32' draws.
fillBelow32M :: PrimMonad m => Word32 -> MU.MVector (PrimState m) Word32 -> Tandem -> m Tandem
fillBelow32M n v g = stToPrim (below32 n v g)

below32 :: Word32 -> MU.MVector s Word32 -> Tandem -> ST s Tandem
below32 n v@(U.MV_Word32 (P.MVector off len mba)) g
  | len == 0 = pure g
  | len >= nativeMin = native 32 len (\p a i c -> keyed g cFillU32Below p a i c n) mba off len g
  | otherwise = do
      g' <- fillWord32M v g
      bound mul32 nextWord32 (fallbackOf purposeBelow32 g) (align (position g) 32 `shiftR` 5) n v
      pure g'

-- | 'fillBelow32M' on 64-bit draws, with @purpose 0x424c573634@.
fillBelow64M :: PrimMonad m => Word64 -> MU.MVector (PrimState m) Word64 -> Tandem -> m Tandem
fillBelow64M n v g = stToPrim (below64 n v g)

below64 :: Word64 -> MU.MVector s Word64 -> Tandem -> ST s Tandem
below64 n v@(U.MV_Word64 (P.MVector off len mba)) g
  | len == 0 = pure g
  | len >= nativeMin = native 64 len (\p a i c -> keyed g cFillU64Below p a i c n) mba off len g
  | otherwise = do
      g' <- fillWord64M v g
      bound mul64 nextWord64 (fallbackOf purposeBelow64 g) (align (position g) 64 `shiftR` 6) n v
      pure g'

-- | Fill with uniform integers in @[0, n)@, with the draw width taken from the range: 32-bit
-- draws when @n <= 2^32@ and 64-bit draws above, so the values do not depend on the result type.
fillBelowM :: PrimMonad m => Word64 -> MU.MVector (PrimState m) Word64 -> Tandem -> m Tandem
fillBelowM n v g = stToPrim (byRange n v g)

byRange :: Word64 -> MU.MVector s Word64 -> Tandem -> ST s Tandem
byRange n v g
  | n > 2 ^ (32 :: Int) = below64 n v g
  | MU.null v = pure g
  | otherwise = do
      narrow <- MU.unsafeNew (MU.length v)
      -- A range of 2^32 accepts every draw and returns it.
      g' <- if n == 2 ^ (32 :: Int) then fillWord32M narrow g else below32 (fromIntegral n) narrow g
      let widen i
            | i == MU.length v = pure ()
            | otherwise = MU.unsafeRead narrow i >>= MU.unsafeWrite v i . fromIntegral >> widen (i + 1)
      widen 0
      pure g'

-- Normals and exponentials ----------------------------------------------------------------

-- | Two standard normals by Box-Muller from two 'Double' draws, the cosine half first.
nextNormalPair :: Tandem -> ((Double, Double), Tandem)
nextNormalPair g0 =
  let !(a, g1) = nextDouble g0
      !(b, g2) = nextDouble g1
      !z = boxMuller a b
   in (z, g2)

-- | 'nextNormalPair' in single precision from two 'Float' draws.
nextNormalPairFloat :: Tandem -> ((Float, Float), Tandem)
nextNormalPairFloat g0 =
  let !(a, g1) = nextFloat g0
      !(b, g2) = nextFloat g1
      !z = boxMullerF a b
   in (z, g2)

-- | A standard normal: the cosine half of 'nextNormalPair'. It consumes two draws and equals
-- element 0 of a fill.
nextNormal :: Tandem -> (Double, Tandem)
nextNormal g = let !((c, _), g') = nextNormalPair g in (c, g')

-- | 'nextNormal' in single precision.
nextNormalFloat :: Tandem -> (Float, Tandem)
nextNormalFloat g = let !((c, _), g') = nextNormalPairFloat g in (c, g')

-- | A standard exponential @-ln(1 - u)@ from one 'Double' draw.
nextExponential :: Tandem -> (Double, Tandem)
nextExponential g = let !(u, g') = nextDouble g; !e = 0.5 * neg2Log (1 - u) in (e, g')

-- | 'nextExponential' in single precision from one 'Float' draw.
nextExponentialFloat :: Tandem -> (Float, Tandem)
nextExponentialFloat g = let !(u, g') = nextFloat g; !e = 0.5 * neg2LogF (1 - u) in (e, g')

-- | Run @fill@ on the first @m@ elements and map them in place. An empty fill leaves the
-- position as it is, unlike a plain fill.
mapped
  :: MU.Unbox a
  => (MU.MVector s a -> Tandem -> ST s Tandem)
  -> (MU.MVector s a -> ST s ())
  -> Int
  -> MU.MVector s a
  -> Tandem
  -> ST s Tandem
mapped fill f m v g
  | m == 0 = pure g
  | otherwise = do
      let b = MU.unsafeSlice 0 m v
      g' <- fill b g
      f b
      pure g'
{-# INLINE mapped #-}

pairs :: MU.Unbox a => (a -> a -> (a, a)) -> MU.MVector s a -> ST s ()
pairs f b = go 0
  where
    go i
      | i + 1 >= MU.length b = pure ()
      | otherwise = do
          x <- MU.unsafeRead b i
          y <- MU.unsafeRead b (i + 1)
          let !(c, s) = f x y
          MU.unsafeWrite b i c
          MU.unsafeWrite b (i + 1) s
          go (i + 2)
{-# INLINE pairs #-}

each :: MU.Unbox a => (a -> a) -> MU.MVector s a -> ST s ()
each f b = go 0
  where
    go i
      | i == MU.length b = pure ()
      | otherwise = MU.unsafeRead b i >>= MU.unsafeWrite b i . f >> go (i + 1)
{-# INLINE each #-}

-- | Fill with standard normals. Pair @j@ is elements @2j@ and @2j + 1@, the cosine half first,
-- from draws @2j@ and @2j + 1@ of the 'Double' fill, so the fill is the flattened sequence of
-- 'nextNormalPair' draws. An odd length writes the cosine half of its last pair and still
-- consumes both draws. An empty fill leaves the position as it is.
fillNormalM :: PrimMonad m => MU.MVector (PrimState m) Double -> Tandem -> m Tandem
fillNormalM v@(U.MV_Double (P.MVector off n mba)) g
  | n >= nativeMin = stToPrim (native 64 (n + n `rem` 2) (keyed g cFillNormalF64) mba off n g)
  | otherwise = stToPrim (normals fillDoubleM boxMuller nextNormal v g)

-- | 'fillNormalM' in single precision from the 'Float' fill.
fillNormalFloatM :: PrimMonad m => MU.MVector (PrimState m) Float -> Tandem -> m Tandem
fillNormalFloatM v@(U.MV_Float (P.MVector off n mba)) g
  | n >= nativeMin = stToPrim (native 32 (n + n `rem` 2) (keyed g cFillNormalF32) mba off n g)
  | otherwise = stToPrim (normals fillFloatM boxMullerF nextNormalFloat v g)

normals
  :: MU.Unbox a
  => (MU.MVector s a -> Tandem -> ST s Tandem)
  -> (a -> a -> (a, a))
  -> (Tandem -> (a, Tandem))
  -> MU.MVector s a
  -> Tandem
  -> ST s Tandem
normals fill pair next v g = do
  let n = MU.length v
  g' <- mapped fill (pairs pair) (n - n `rem` 2) v g
  if odd n
    then let !(z, g'') = next g' in MU.unsafeWrite v (n - 1) z >> pure g''
    else pure g'
{-# INLINE normals #-}

-- | Fill with standard exponentials. Element @i@ comes from draw @i@ of the 'Double' fill, so the
-- fill equals the scalar 'nextExponential' draws. An empty fill leaves the position as it is.
fillExponentialM :: PrimMonad m => MU.MVector (PrimState m) Double -> Tandem -> m Tandem
fillExponentialM v@(U.MV_Double (P.MVector off n mba)) g
  | n >= nativeMin = stToPrim (native 64 n (keyed g cFillExponentialF64) mba off n g)
  | otherwise = stToPrim (mapped fillDoubleM (each (\u -> 0.5 * neg2Log (1 - u))) n v g)

-- | 'fillExponentialM' in single precision from the 'Float' fill.
fillExponentialFloatM :: PrimMonad m => MU.MVector (PrimState m) Float -> Tandem -> m Tandem
fillExponentialFloatM v@(U.MV_Float (P.MVector off n mba)) g
  | n >= nativeMin = stToPrim (native 32 n (keyed g cFillExponentialF32) mba off n g)
  | otherwise = stToPrim (mapped fillFloatM (each (\u -> 0.5 * neg2LogF (1 - u))) n v g)
