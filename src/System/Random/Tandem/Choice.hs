-- | Weighted choice, Appendix C of the specification: an alias table in exact integers, so that
-- every port builds the same table and returns the same indices.
module System.Random.Tandem.Choice
  ( Choice
  , choice
  , choiceSize
  , choiceCapacity
  , choiceCuts
  , choiceAliases
  , nextChoice
  , fillChoiceM
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST, runST)
import Data.Bits (countLeadingZeros, shiftL, shiftR, (.&.), (.|.))
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64)

import System.Random.Tandem.Generator

-- | An alias table: column @j@ of @m@ keeps @cut j@ of its capacity for index @j@ and gives the
-- rest to @alias j@. Building draws nothing.
data Choice = Choice
  { chSize :: !Word64
  , chCapacity :: !Word64
  , chCut :: !(U.Vector Word64)
  , chAlias :: !(U.Vector Word32)
  }
  deriving (Eq, Show)

-- | The number of weights.
choiceSize :: Choice -> Int
choiceSize = fromIntegral . chSize

-- | The column capacity @S@.
choiceCapacity :: Choice -> Word64
choiceCapacity = chCapacity

-- | The cut of every column.
choiceCuts :: Choice -> U.Vector Word64
choiceCuts = chCut

-- | The alias of every column.
choiceAliases :: Choice -> U.Vector Word32
choiceAliases = chAlias

bitLength :: Word64 -> Int
bitLength x = 64 - countLeadingZeros x

-- | The integer significand and the exponent of a finite non-negative weight, @w = s 2^e@.
decompose :: Double -> (Word64, Int)
decompose w
  | biased == 0 = (frac, -1074)
  | otherwise = (frac .|. (1 `shiftL` 52), biased - 1075)
  where
    bits = castDoubleToWord64 w
    biased = fromIntegral ((bits `shiftR` 52) .&. 0x7ff)
    frac = bits .&. 0xfffffffffffff

-- | @ceil (s 2^(e + t))@ for a significand @s@ below @2^53@, exact. The caller keeps it below
-- @2^64@.
ceilScaled :: (Word64, Int) -> Int -> Word64
ceilScaled (s, e) t
  | s == 0 = 0
  | k >= 0 = s `shiftL` k
  | k <= -54 = 1
  | otherwise = (s `shiftR` negate k) + (if s .&. ((1 `shiftL` negate k) - 1) /= 0 then 1 else 0)
  where
    k = e + t

-- | The table of the weights, or 'Nothing' unless there are 1 to @2^32 - 1@ of them, all finite
-- and not negative, and at least one is positive. @-0.0@ is zero.
choice :: U.Vector Double -> Maybe Choice
choice ws
  | m == 0 || m > 0xffffffff = Nothing
  | U.any (\w -> isNaN w || isInfinite w || w < 0) ws = Nothing
  | wmax == 0 = Nothing
  | otherwise = Just (Choice (fromIntegral m) s cut alias)
  where
    m = U.length ws
    mw = fromIntegral m :: Word64
    wmax = U.maximum ws
    parts = U.map decompose ws
    -- 2^e <= wmax < 2^(e + 1), then a first pass that cannot overflow and a second that scales
    -- the total to just below 2^63.
    (ms, me) = decompose wmax
    e = bitLength ms - 1 + me
    t0 = 63 - bitLength mw - e
    a = U.foldl' (\acc p -> acc + ceilScaled p t0) 0 parts
    t = t0 + 63 - bitLength a
    q0 = U.map (`ceilScaled` t) parts
    total = U.foldl' (+) 0 q0
    -- The first index of the largest mass takes the padding that makes the total a multiple of m.
    big = U.maxIndex q0
    s = (total + mw - 1) `quot` mw
    masses = q0 U.// [(big, q0 U.! big + (s * mw - total))]
    (cut, alias) = runST $ do
      c <- U.thaw masses
      al <- U.thaw (U.generate m fromIntegral :: U.Vector Word32)
      pair s c al
      (,) <$> U.unsafeFreeze c <*> U.unsafeFreeze al

-- | Pair the columns in place by Vose's method: the first full column @l@ fills each short
-- column.
pair :: Word64 -> MU.MVector s Word64 -> MU.MVector s Word32 -> ST s ()
pair s c al = firstFull 0 >>= outer 0
  where
    m = MU.length c
    firstFull k
      | k >= m = pure m
      | otherwise = MU.unsafeRead c k >>= \x -> if x >= s then pure k else firstFull (k + 1)
    outer i l
      | i == m = pure ()
      | otherwise = inner i i l >>= outer (i + 1)
    inner i j l
      | j > i = pure l
      | otherwise = do
          cj <- MU.unsafeRead c j
          if cj >= s
            then pure l
            else do
              MU.unsafeWrite al j (fromIntegral l)
              cl <- MU.unsafeRead c l
              let cl' = cl - (s - cj)
              MU.unsafeWrite c l cl'
              if cl' < s then firstFull (l + 1) >>= inner i l else inner i l l

-- | The index of the 64-bit draw @r@: the column from the high word of @r m@, and the low word
-- scaled by the capacity against the column's cut.
indexOf :: Choice -> Word64 -> Word32
indexOf (Choice m s cut alias) r =
  let (hi, lo) = mul64 r m
      (v, _) = mul64 lo s
      j = fromIntegral hi
   in if v < U.unsafeIndex cut j then fromIntegral j else U.unsafeIndex alias j
{-# INLINE indexOf #-}

-- | An index from one 64-bit draw. It equals element 0 of 'fillChoiceM'.
nextChoice :: Choice -> Tandem -> (Word32, Tandem)
nextChoice t g = let !(r, g') = nextWord64 g in (indexOf t r, g')
{-# INLINE nextChoice #-}

-- | Fill with indices. Element @i@ is the index of draw @i@ of the 64-bit fill, with no retry,
-- so a fill cut at any element equals the whole fill. An empty fill aligns the position to 64
-- bits.
fillChoiceM :: PrimMonad m => Choice -> MU.MVector (PrimState m) Word32 -> Tandem -> m Tandem
fillChoiceM t v g0 = stToPrim $ do
  let n = MU.length v
      block = min n 4096
  -- Blocks above the size that tandem-c's fill takes over.
  raw <- MU.unsafeNew block
  let go i g
        | i >= n = pure g
        | otherwise = do
            let k = min block (n - i)
                r = MU.unsafeSlice 0 k raw
            g' <- fillWord64M r g
            let map' j
                  | j == k = pure ()
                  | otherwise = MU.unsafeRead r j >>= MU.unsafeWrite v (i + j) . indexOf t >> map' (j + 1)
            map' 0
            go (i + k) g'
  if n == 0 then fillWord64M raw g0 else go 0 g0
