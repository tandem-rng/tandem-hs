-- | The building blocks of the specification: the step @T@, the seeding function @F@ and the
-- stream blocks. "System.Random.Tandem" builds the generator on them.
module System.Random.Tandem.Core
  ( Quad (..)
  , step
  , seedingF
  , keyedF
  , block
    -- * Constants
  , domainStream
  , domainSplit
  , domainFork
  , domainFold
  , domainSeed
  , auxStream
    -- * Unboxed forms
  , stepWith
  , keyedFWith
  ) where

import Data.Bits (rotateL, shiftR, xor, (.|.))
import Data.Word (Word32, Word64)

-- | Four 32-bit words: a key, or one half of a state.
data Quad = Quad !Word32 !Word32 !Word32 !Word32
  deriving (Eq, Ord, Show)

domainStream, domainSplit, domainFork, domainFold, domainSeed, auxStream :: Word32
domainStream = 0x9e3779b9
domainSplit = 0xbb67ae85
domainFork = 0xd2511f53
domainFold = 0xcd9e8d57
domainSeed = 0xa54ff53a
auxStream = 0x94d049bb

clockWeyl :: Word32
clockWeyl = 0x9e3779b9

-- | The step @T@ on the state @(o, h)@.
step :: Quad -> Quad -> (Quad, Quad)
step (Quad a b c d) (Quad h0 h1 h2 h3) =
  stepWith (\o0 o1 o2 o3 g0 g1 g2 g3 -> (Quad o0 o1 o2 o3, Quad g0 g1 g2 g3)) a b c d h0 h1 h2 h3

-- | 'step' on the eight words of a state, passed to a continuation so that the row loops keep
-- the state in registers.
stepWith
  :: (Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> r)
  -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> r
stepWith k !a !b !c !d !h0 !h1 !h2 !h3 =
  let !p0 = wide a * wide (h0 .|. 1)
      !p1 = wide c * wide (h1 .|. 1)
      !n0 = b `xor` hi p1 `xor` lo p1
      !n1 = (lo p1 `rotateL` 16) `xor` h2
      !n2 = d `xor` hi p0 `xor` lo p0
      !n3 = (lo p0 `rotateL` 16) `xor` h3
      !g0 = h0 `xor` (h1 `rotateL` 7)
      !g1 = h1 `xor` (h2 `rotateL` 13)
      !g2 = h2 `xor` (h3 `rotateL` 22)
      !g3 = h3 `xor` (g0 `rotateL` 3)
   in k n0 n1 n2 n3 ((g0 + clockWeyl) `xor` n0) g1 g2 g3
{-# INLINE stepWith #-}

wide :: Word32 -> Word64
wide = fromIntegral
{-# INLINE wide #-}

lo, hi :: Word64 -> Word32
lo = fromIntegral
hi p = fromIntegral (p `shiftR` 32)
{-# INLINE lo #-}
{-# INLINE hi #-}

-- | The seeding function @F@: eight rounds of @T@, a round constant, and a swap of the halves.
seedingF :: Quad -> Quad -> (Quad, Quad)
seedingF (Quad a b c d) (Quad h0 h1 h2 h3) =
  fWith (\o0 o1 o2 o3 g0 g1 g2 g3 -> (Quad o0 o1 o2 o3, Quad g0 g1 g2 g3)) a b c d h0 h1 h2 h3

fWith
  :: (Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> r)
  -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> r
fWith k = rnd 0xd17cc1b7 (rnd 0xa7220a94 (rnd 0xfe13abe8 (rnd 0xfa9a6ee0
          (rnd 0xedb14acc (rnd 0x9e21c820 (rnd 0xff28b1d5 (rnd 0xef5de2b0 k)))))))
  where
    rnd rc next = stepWith (\o0 o1 o2 o3 g0 g1 g2 g3 -> next g0 g1 g2 g3 (o0 `xor` rc) o1 o2 o3)
    {-# INLINE rnd #-}
{-# INLINE fWith #-}

-- | @F(key, counter, domain, aux)@.
keyedF :: Quad -> Word64 -> Word32 -> Word32 -> (Quad, Quad)
keyedF key counter domain aux =
  keyedFWith (\o0 o1 o2 o3 g0 g1 g2 g3 -> (Quad o0 o1 o2 o3, Quad g0 g1 g2 g3)) key counter domain aux

-- | 'keyedF' passed to a continuation.
keyedFWith
  :: (Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> r)
  -> Quad -> Word64 -> Word32 -> Word32 -> r
keyedFWith k (Quad k0 k1 k2 k3) counter domain aux =
  fWith k (lo counter) (hi counter) domain aux k0 k1 k2 k3
{-# INLINE keyedFWith #-}

-- | Block @B(c, j)@: the exposed half of chunk @c@ after @j + 1@ steps.
block :: Quad -> Word64 -> Word32 -> Quad
block key c j = keyedFWith (go (fromIntegral j + 1 :: Int)) key c domainStream auxStream
  where
    go 0 o0 o1 o2 o3 _ _ _ _ = Quad o0 o1 o2 o3
    go n o0 o1 o2 o3 h0 h1 h2 h3 = stepWith (go (n - 1)) o0 o1 o2 o3 h0 h1 h2 h3
