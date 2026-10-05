{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The generator: transport form, row cache, uniform and bounded draws, fills, derived
-- generators, and the instances of the random package.
module System.Random.Tandem.Generator
  ( Tandem (..)
  , defaultChunkLength
  , seed
  , fromKey
  , key
  , chunkLength
  , position
  , seek
  , split
  , fork
  , purpose
  , restart
  , align
  , nextBits
  , nextWord32
  , nextWord64
  , nextDouble
  , nextFloat
  , nextBelow32
  , nextBelow64
  , mul32
  , mul64
  , lemire
  , runC
  , fillWord32M
  , fillWord64M
  , fillDoubleM
  , fillFloatM
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Bits (complement, countTrailingZeros, popCount, shiftL, shiftR, unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.Primitive.ByteArray (MutableByteArray (..), writeByteArray)
import Data.Primitive.PrimArray
  ( MutablePrimArray (..)
  , PrimArray
  , copyPrimArray
  , emptyPrimArray
  , indexPrimArray
  , newPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import Data.Vector.Primitive.Mutable qualified as P
import Data.Vector.Unboxed.Base qualified as U
import Data.Word (Word32, Word64)
import Foreign.C.Types (CInt)
import GHC.Exts (Int (I#), timesWord2#, word64ToWord#, wordToWord64#, writeWord8ArrayAsWord64#)
import GHC.ByteOrder (ByteOrder (LittleEndian), targetByteOrder)
import GHC.ST (ST (..))
import GHC.Float (int2Double, int2Float)
import GHC.Word (Word64 (W64#))
import System.Random qualified as R

import System.Random.Tandem.Core
import System.Random.Tandem.Native
import System.Random.Tandem.Rounds (Lane, keyedFW, stepW)

-- | A Tandem8x32 generator. It is its transport form, the key, the bit position and the chunk
-- length @K@, plus a cache of the current 1024-bit row. Equality and 'Show' cover the transport
-- form only.
data Tandem = Tandem
  { tKey :: {-# UNPACK #-} !Quad
  , tPos :: {-# UNPACK #-} !Word64
  , tK :: {-# UNPACK #-} !Word32
  , tRow :: {-# UNPACK #-} !Word64
  -- ^ The row in the cache, or 'noRow'.
  , tCache :: {-# UNPACK #-} !(PrimArray Word32)
  -- ^ With tandem-c, its tandem_rng in words 0 to 'stateWords' - 1, then the 32 stream words of
  -- row 'tRow' if there is one. Without it, the eight lane states of row 'tRow': word @w@ of lane
  -- @l@ at @8 l + w@, @o@ in words 0 to 3 and @h@ in words 4 to 7.
  }

instance Eq Tandem where
  a == b = tKey a == tKey b && tPos a == tPos b && tK a == tK b

instance Show Tandem where
  showsPrec d g =
    showParen (d > 10) $
      showString "fromKey " . showsPrec 11 (tKey g) . showChar ' ' . showsPrec 11 (tPos g)
        . showChar ' ' . showsPrec 11 (tK g)

-- | The default chunk length, @K = 32@.
defaultChunkLength :: Word32
defaultChunkLength = 32

-- Row indices are below 2^54, so this never names a row.
noRow :: Word64
noRow = maxBound

-- | A generator from an integer seed in @[0, 2^128)@ through the specification's seed
-- whitening, at position 0 with @K = 32@.
seed :: Integer -> Tandem
seed z
  | z < 0 || z >= 2 ^ (128 :: Int) = error "System.Random.Tandem.seed: seed must be in [0, 2^128)"
  | otherwise = restart (fst (seedingF (Quad 0 0 domainSeed 0) (Quad (w 0) (w 32) (w 64) (w 96)))) defaultChunkLength
  where
    w s = fromInteger (z `shiftR` s)

-- | A generator from its transport form: key, bit position below @2^63@ and chunk length @K@, a
-- power of two in @[1, 65536]@.
fromKey :: Quad -> Word64 -> Word32 -> Tandem
fromKey k p kk
  | kk == 0 || kk > 65536 || popCount kk /= 1 =
      error "System.Random.Tandem.fromKey: chunk length must be a power of two in [1, 65536]"
  | otherwise = seek p (restart k kk)

-- | A generator at position 0 with no cache.
restart :: Quad -> Word32 -> Tandem
restart k kk = Tandem k 0 kk noRow emptyPrimArray

-- | The 128-bit key.
key :: Tandem -> Quad
key = tKey

-- | The chunk length @K@.
chunkLength :: Tandem -> Word32
chunkLength = tK

-- | The stream bit position of the next draw, before alignment.
position :: Tandem -> Word64
position = tPos

-- | Move to a stream bit position below @2^63@. The cache stays, so a move inside the current
-- chunk group costs nothing until the next draw.
seek :: Word64 -> Tandem -> Tandem
seek p g
  | p >= 2 ^ (63 :: Int) = error "System.Random.Tandem.seek: position must be below 2^63"
  | otherwise = g {tPos = p}

-- Derived generators ----------------------------------------------------------------------

child :: Tandem -> Word64 -> Word32 -> Word32 -> Bool -> Tandem
child g counter domain aux hidden =
  let (o, h) = keyedF (tKey g) counter domain aux
   in restart (if hidden then h else o) (tK g)

-- | Child @i@ by key alone: the same child for the same index, whatever the position.
split :: Word64 -> Tandem -> Tandem
split i g = child g (i `shiftR` 1) domainSplit 0 (odd i)

-- | The child for a named purpose, by key alone.
purpose :: Word64 -> Tandem -> Tandem
purpose u g = child g u domainFold 0 False

-- | Fork @n <= 2^33@ children from the current 128-bit block and move the parent past it, for an
-- empty batch too. Successive forks give fresh children.
fork :: Word64 -> Tandem -> ([Tandem], Tandem)
fork n g
  | n > 2 ^ (33 :: Int) = error "System.Random.Tandem.fork: batch size must be at most 2^33"
  | otherwise = (kids, g {tPos = (b + 1) `shiftL` 7})
  where
    b = tPos g `shiftR` 7
    kids = [child g b domainFork (fromIntegral (i `shiftR` 1)) (odd i) | i <- takeWhile (< n) [0 ..]]

-- Rows ------------------------------------------------------------------------------------

-- Strict in the words, so that the loop keeps them unboxed until it calls @k@.
stepTimes :: Word64 -> Lane r -> Lane r
stepTimes n0 k = go n0
  where
    go 0 !a !b !c !d !e !f !g !h = k a b c d e f g h
    go n !a !b !c !d !e !f !g !h = stepW (go (n - 1)) a b c d e f g h
{-# INLINE stepTimes #-}

-- | The state of lane @l@ at row @row@: stepped forward from the cache when it holds an earlier
-- row of the same chunk group, else seeded.
withLane :: Tandem -> Word64 -> Int -> Lane r -> r
withLane g row l k
  | forward row g =
      stepTimes (row - tRow g) k (c 0) (c 1) (c 2) (c 3) (c 4) (c 5) (c 6) (c 7)
  | otherwise =
      let Quad k0 k1 k2 k3 = tKey g
       in keyedFW
            (stepTimes ((row .&. fromIntegral (tK g - 1)) + 1) k)
            k0 k1 k2 k3
            (8 * (row `unsafeShiftR` countTrailingZeros (tK g)) + fromIntegral l)
            domainStream
            auxStream
  where
    c w = fromIntegral (indexPrimArray (tCache g) (8 * l + w))
{-# INLINE withLane #-}

saveLane :: MutablePrimArray s Word32 -> Int -> Lane (ST s ())
saveLane m l a b c d e f g h = do
  writePrimArray m (8 * l) (fromIntegral a)
  writePrimArray m (8 * l + 1) (fromIntegral b)
  writePrimArray m (8 * l + 2) (fromIntegral c)
  writePrimArray m (8 * l + 3) (fromIntegral d)
  writePrimArray m (8 * l + 4) (fromIntegral e)
  writePrimArray m (8 * l + 5) (fromIntegral f)
  writePrimArray m (8 * l + 6) (fromIntegral g)
  writePrimArray m (8 * l + 7) (fromIntegral h)
{-# INLINE saveLane #-}

-- | Produce rows @[row0, row0 + nrows)@, @nrows > 0@, through @emit i lane o0 o1 o2 o3@ with
-- @i@ counted from 0, and cache the last. Lanes run one at a time so that a lane's state stays
-- in registers for a whole chunk.
runRows
  :: Tandem
  -> Word64
  -> Int
  -> (Int -> Int -> Word -> Word -> Word -> Word -> ST s ())
  -> ST s Tandem
runRows g row0 nrows emit = do
  m <- newPrimArray 64
  let k = fromIntegral (tK g) :: Int
      runs row done
        | done >= nrows = pure ()
        | otherwise = do
            let run = min (nrows - done) (k - fromIntegral (row .&. fromIntegral (k - 1)))
                lanes l
                  | l == 8 = pure ()
                  | otherwise = withLane g row l (laneLoop l done run 0) >> lanes (l + 1)
            lanes 0
            runs (row + fromIntegral run) (done + run)
      laneLoop l done run = go
        where
          go !r !a !b !c !d !e !f !gg !h = do
            emit (done + r) l a b c d
            if r + 1 < run
              then stepW (go (r + 1)) a b c d e f gg h
              else saveLane m l a b c d e f gg h
  runs row0 0
  cache <- unsafeFreezePrimArray m
  pure g {tRow = row0 + fromIntegral nrows - 1, tCache = cache}
{-# INLINE runRows #-}

-- | Load row @r@ into the cache.
loadRow :: Word64 -> Tandem -> Tandem
loadRow r g
  | tRow g == r = g
  | native = refill r g
  | otherwise = advance r g
{-# INLINE loadRow #-}

-- | Load row @r@ through tandem-c. The cache holds the tandem_rng in words 0 to
-- 'stateWords' - 1 and the 32 stream words of row 'tRow' after them. A row after the cached one
-- in the same chunk group costs one step per row in C, and the row of the cache costs none.
refill :: Word64 -> Tandem -> Tandem
refill r g = runST $ do
  m@(MutablePrimArray mm) <- newPrimArray (stateWords + 32)
  fresh <- loadState g m
  let Quad k0 k1 k2 k3 = tKey g
  _ <- unsafeIOToST (cRun mm fresh k0 k1 k2 k3 (tK g) (r `shiftL` 10) (kind KU32) mm (fromIntegral stateWords) 32 0)
  cache <- unsafeFreezePrimArray m
  pure g {tRow = r, tCache = cache}
{-# NOINLINE refill #-}

-- | Copy the tandem_rng of the cache into @m@. The result is 1 when the cache holds none, which
-- tells tandem-c to start from the transport form.
loadState :: Tandem -> MutablePrimArray s Word32 -> ST s CInt
loadState g m
  | sizeofPrimArray (tCache g) >= stateWords = copyPrimArray m 0 (tCache g) 0 stateWords >> pure 0
  | otherwise = pure 1
{-# INLINE loadState #-}

kind :: Kind -> CInt
kind = fromIntegral . fromEnum
{-# INLINE kind #-}

-- Sequential draws step every lane forward inside the group, the common case. Anything else
-- goes through 'runRows', which seeds.
advance :: Word64 -> Tandem -> Tandem
advance r g
  | forward r g = runST $ do
      m <- newPrimArray 64
      let lanes l
            | l == 8 = pure ()
            | otherwise = withLane g r l (saveLane m l) >> lanes (l + 1)
      lanes 0
      cache <- unsafeFreezePrimArray m
      pure g {tRow = r, tCache = cache}
  | otherwise = runST (runRows g r 1 (\_ _ _ _ _ _ -> pure ()))
{-# NOINLINE advance #-}

-- | Whether the cache holds an earlier row of the group of row @r@.
forward :: Word64 -> Tandem -> Bool
forward r g =
  tRow g /= noRow && r >= tRow g && r `unsafeShiftR` s == tRow g `unsafeShiftR` s
  where
    s = countTrailingZeros (tK g)
{-# INLINE forward #-}

-- Reads -----------------------------------------------------------------------------------

-- | The position aligned up to a power of two @w@.
align :: Word64 -> Int -> Word64
align p w = (p + fromIntegral w - 1) .&. complement (fromIntegral w - 1)
{-# INLINE align #-}

-- | The stream word at bit position @p@ of the loaded row: in tandem-c's cache the row's words
-- in stream order, in Haskell's the @o@ words of the lane states.
wordAt :: PrimArray Word32 -> Word64 -> Word32
wordAt c p
  | native = indexPrimArray c (stateWords + fromIntegral ((p `shiftR` 5) .&. 31))
  | otherwise = indexPrimArray c (fromIntegral (((p `shiftR` 4) .&. 0x38) .|. ((p `shiftR` 5) .&. 3)))
{-# INLINE wordAt #-}

-- | The @w@ bits at the aligned position @p@, @w@ a power of two from 1 to 64, with the row
-- loaded.
readAt :: Int -> Word64 -> Tandem -> (Word64, Tandem)
readAt w p g0 =
  let !g = loadRow (p `shiftR` 10) g0
      !c = tCache g
      !v
        | w == 64 = fromIntegral (wordAt c p) .|. (fromIntegral (wordAt c (p + 32)) `shiftL` 32)
        | otherwise =
            fromIntegral ((wordAt c p `shiftR` fromIntegral (p .&. 31)) .&. (maxBound `shiftR` (32 - w)))
   in (v, g)
{-# INLINE readAt #-}

-- | Draw @w@ bits: align, read, advance.
nextBits :: Int -> Tandem -> (Word64, Tandem)
nextBits w g =
  let !p = align (tPos g) w
      !(v, g') = readAt w p g
      !g'' = g' {tPos = p + fromIntegral w}
   in (v, g'')
{-# INLINE nextBits #-}

-- | An aligned 32-bit draw.
nextWord32 :: Tandem -> (Word32, Tandem)
nextWord32 g = let !(v, g') = nextBits 32 g; !x = fromIntegral v in (x, g')
{-# INLINE nextWord32 #-}

-- | An aligned 64-bit draw.
nextWord64 :: Tandem -> (Word64, Tandem)
nextWord64 = nextBits 64
{-# INLINE nextWord64 #-}

-- | A uniform 'Double' in @[0, 1)@: @(raw >> 11) 2^-53@ of a 64-bit draw.
nextDouble :: Tandem -> (Double, Tandem)
nextDouble g = let !(v, g') = nextBits 64 g; !x = toDouble v in (x, g')
{-# INLINE nextDouble #-}

-- | A uniform 'Float' in @[0, 1)@: @(raw >> 8) 2^-24@ of a 32-bit draw.
nextFloat :: Tandem -> (Float, Tandem)
nextFloat g = let !(v, g') = nextBits 32 g; !x = toFloat (fromIntegral v) in (x, g')
{-# INLINE nextFloat #-}

-- Bounded integers ------------------------------------------------------------------------

-- | The high and low words of the full product.
mul64 :: Word64 -> Word64 -> (Word64, Word64)
mul64 (W64# a) (W64# b) = case timesWord2# (word64ToWord# a) (word64ToWord# b) of
  (# h, l #) -> (W64# (wordToWord64# h), W64# (wordToWord64# l))
{-# INLINE mul64 #-}

mul32 :: Word32 -> Word32 -> (Word32, Word32)
mul32 a b =
  let m = fromIntegral a * fromIntegral b :: Word64 in (fromIntegral (m `shiftR` 32), fromIntegral m)
{-# INLINE mul32 #-}

-- | Lemire's multiply and reject over the draws of @next@: the first accepted value in @[0, n)@.
-- For @n = 0@ the result is 0 after one draw.
lemire :: Integral w => (w -> w -> (w, w)) -> (Tandem -> (w, Tandem)) -> w -> Tandem -> (w, Tandem)
lemire mul next n = go
  where
    -- Only evaluated for n > 0, where a low word below n can be rejected.
    t = negate n `rem` n
    go g =
      let !(x, g') = next g
          !(h, l) = mul x n
       in if l < n && l < t then go g' else (h, g')
{-# INLINE lemire #-}

-- | A uniform integer in @[0, n)@ by Lemire's multiply and reject on 32-bit draws. A rejection
-- draws the next 32 bits of the stream. For @n = 0@ the result is 0 after one draw.
nextBelow32 :: Word32 -> Tandem -> (Word32, Tandem)
nextBelow32 = lemire mul32 nextWord32
{-# INLINE nextBelow32 #-}

-- | 'nextBelow32' on 64-bit draws.
nextBelow64 :: Word64 -> Tandem -> (Word64, Tandem)
nextBelow64 = lemire mul64 nextWord64
{-# INLINE nextBelow64 #-}

-- | The draws of the widths the methods name. Bounded draws take Lemire's method of Appendix A,
-- with the draw width from the range, so @genWord64R@ with a range up to @2^32@ consumes 32-bit
-- draws and equals @genWord32R@.
instance R.RandomGen Tandem where
  genWord8 g = let !(v, g') = nextBits 8 g; !x = fromIntegral v in (x, g')
  genWord16 g = let !(v, g') = nextBits 16 g; !x = fromIntegral v in (x, g')
  genWord32 = nextWord32
  genWord64 = nextWord64
  genWord32R m g
    | m == maxBound = nextWord32 g
    | otherwise = nextBelow32 (m + 1) g
  genWord64R m g
    | m == maxBound = nextWord64 g
    | m > 0xffffffff = nextBelow64 (m + 1) g
    | otherwise =
        let !(v, g') = if m == 0xffffffff then nextWord32 g else nextBelow32 (fromIntegral m + 1) g
            !x = fromIntegral v
         in (x, g')

-- | @splitGen g@ is the parent after @fork 1@ and its child.
instance R.SplitGen Tandem where
  splitGen g = (g {tPos = (b + 1) `shiftL` 7}, child g b domainFork 0 False)
    where
      b = tPos g `shiftR` 7

toDouble :: Word64 -> Double
toDouble raw = int2Double (fromIntegral (raw `shiftR` 11)) * 0x1p-53
{-# INLINE toDouble #-}

toFloat :: Word32 -> Float
toFloat raw = int2Float (fromIntegral (raw `shiftR` 8)) * 0x1p-24
{-# INLINE toFloat #-}

-- Fills -----------------------------------------------------------------------------------

-- | Writes the 128 bits of one block, four 32-bit words held in 'Word's, as elements from index
-- @i@.
type PutBlock s = MutableByteArray s -> Int -> Word -> Word -> Word -> Word -> ST s ()

-- | Run the tandem-c fill of @k@ (with bound @range@) on elements @[off, off + n)@ of the array,
-- continuing the tandem_rng of the cache, in pieces of 2^20 elements so that no unsafe call
-- delays a garbage collection for long. Pieces join exactly, since every fill ends where the
-- next one starts and the pieces have even lengths. The end of the @draws@ draws of @w@ bits is
-- checked before anything is written. The generator keeps the tandem_rng without a loaded row.
runC :: Kind -> Word64 -> Int -> Int -> MutableByteArray s -> Int -> Int -> Tandem -> ST s Tandem
runC k range w draws (MutableByteArray out) off n g = start w draws (tPos g) `seq` do
  m@(MutablePrimArray mm) <- newPrimArray stateWords
  fresh <- loadState g m
  let Quad k0 k1 k2 k3 = tKey g
      end = off + n
      go i p f
        | i >= end = pure p
        | otherwise = do
            let c = min (2 ^ (20 :: Int)) (end - i)
            p' <- unsafeIOToST (cRun mm f k0 k1 k2 k3 (tK g) p (kind k) out (fromIntegral i) (fromIntegral c) range)
            go (i + c) p' 0
  p <- go off (tPos g) fresh
  cache <- unsafeFreezePrimArray m
  pure g {tPos = p, tRow = noRow, tCache = cache}
{-# INLINE runC #-}

-- | The fill of @w@-bit elements: the values of as many scalar draws.
fillPrim :: Prim a => Int -> (Word64 -> a) -> PutBlock s -> Kind -> P.MVector s a -> Tandem -> ST s Tandem
fillPrim w conv putBlock k v@(P.MVector off n mba) g
  | native && n > 0 = runC k 0 w n mba off n g
  | otherwise = fillRows w conv putBlock v g
{-# INLINE fillPrim #-}

-- | 'fillPrim' in Haskell. After alignment the stream is read in whole rows, straight into the
-- output.
fillRows :: Prim a => Int -> (Word64 -> a) -> PutBlock s -> P.MVector s a -> Tandem -> ST s Tandem
fillRows w conv putBlock (P.MVector off n mba) g0 = do
  let p0 = start w n (tPos g0)
      perRow = 1024 `quot` w
      nh = min n (fromIntegral ((1024 - (p0 .&. 1023)) `quot` fromIntegral w) `rem` perRow)
      nrows = (n - nh) `quot` perRow
      p1 = p0 + fromIntegral (w * nh)
      nt = n - nh - nrows * perRow
      scalars i cnt p g
        | i == cnt = pure g
        | otherwise = do
            let !(v, g') = readAt w p g
            writeByteArray mba i (conv v)
            scalars (i + 1) cnt (p + fromIntegral w) g'
  g1 <- scalars off (off + nh) p0 g0
  g2 <-
    if nrows == 0
      then pure g1
      else runRows g1 (p1 `shiftR` 10) nrows $ \r l ->
        putBlock mba (off + nh + r * perRow + l * (perRow `quot` 8))
  let tailAt = off + nh + nrows * perRow
  g3 <- scalars tailAt (tailAt + nt) (p1 + 1024 * fromIntegral nrows) g2
  pure g3 {tPos = p0 + fromIntegral w * fromIntegral n}
{-# INLINE fillRows #-}

-- | The aligned start of a fill of @n@ elements of @w@ bits, checked so that the fill ends below
-- @2^64@.
start :: Int -> Int -> Word64 -> Word64
start w n p
  -- The aligned start wraps to 0 when it would reach 2^64. A count of 2^57 or more elements
  -- cannot be allocated, and keeps the product below 2^63.
  | a < p || n >= 2 ^ (57 :: Int) || (a /= 0 && fromIntegral w * fromIntegral n >= negate a) =
      error "System.Random.Tandem: fill ends beyond bit position 2^64"
  | otherwise = a
  where
    a = align p w
{-# INLINE start #-}

-- Two 64-bit stores in place of four 32-bit ones on a little-endian target, where they lay the
-- words out in order. They are unaligned, as a slice of a vector can start at an odd element.
put32 :: PutBlock s
put32 mba i a b c d
  | targetByteOrder == LittleEndian = do
      writeUnaligned64 mba (4 * i) (pair64 a b)
      writeUnaligned64 mba (4 * i + 8) (pair64 c d)
  | otherwise = do
      writeByteArray mba i (fromIntegral a :: Word32)
      writeByteArray mba (i + 1) (fromIntegral b :: Word32)
      writeByteArray mba (i + 2) (fromIntegral c :: Word32)
      writeByteArray mba (i + 3) (fromIntegral d :: Word32)
{-# INLINE put32 #-}

-- | A 64-bit store at byte offset @o@, which need not be a multiple of 8.
writeUnaligned64 :: MutableByteArray s -> Int -> Word64 -> ST s ()
writeUnaligned64 (MutableByteArray m) (I# o) (W64# x) = ST (\s -> (# writeWord8ArrayAsWord64# m o x s, () #))
{-# INLINE writeUnaligned64 #-}

pair64 :: Word -> Word -> Word64
pair64 a b = fromIntegral (a .|. (b `unsafeShiftL` 32))
{-# INLINE pair64 #-}

-- | Fill a vector with 32-bit draws.
fillWord32M :: PrimMonad m => U.MVector (PrimState m) Word32 -> Tandem -> m Tandem
fillWord32M (U.MV_Word32 v) g = stToPrim (fillPrim 32 fromIntegral put32 KU32 v g)

-- | Fill a vector with 64-bit draws.
fillWord64M :: PrimMonad m => U.MVector (PrimState m) Word64 -> Tandem -> m Tandem
fillWord64M (U.MV_Word64 v) g = stToPrim (fillPrim 64 id put KU64 v g)
  where
    put mba i a b c d = do
      writeByteArray mba i (pair64 a b)
      writeByteArray mba (i + 1) (pair64 c d)

-- | Fill a vector with uniform 'Double's in @[0, 1)@.
fillDoubleM :: PrimMonad m => U.MVector (PrimState m) Double -> Tandem -> m Tandem
fillDoubleM (U.MV_Double v) g = stToPrim (fillPrim 64 toDouble put KF64 v g)
  where
    put mba i a b c d = do
      writeByteArray mba i (toDouble (pair64 a b))
      writeByteArray mba (i + 1) (toDouble (pair64 c d))

-- | Fill a vector with uniform 'Float's in @[0, 1)@.
fillFloatM :: PrimMonad m => U.MVector (PrimState m) Float -> Tandem -> m Tandem
fillFloatM (U.MV_Float v) g = stToPrim (fillPrim 32 (toFloat . fromIntegral) put KF32 v g)
  where
    put mba i a b c d = do
      writeByteArray mba i (toFloatW a)
      writeByteArray mba (i + 1) (toFloatW b)
      writeByteArray mba (i + 2) (toFloatW c)
      writeByteArray mba (i + 3) (toFloatW d)
    -- 'toFloat' on a word below 2^32 held in a 'Word', without narrowing it first.
    toFloatW x = int2Float (fromIntegral (x `unsafeShiftR` 8)) * 0x1p-24
