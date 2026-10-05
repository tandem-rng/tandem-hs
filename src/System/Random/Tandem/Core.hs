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

import Data.Word (Word32, Word64)

import System.Random.Tandem.Rounds (Lane, fW, keyedFW, stepW)

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

-- | The step @T@ on the state @(o, h)@.
step :: Quad -> Quad -> (Quad, Quad)
step (Quad a b c d) (Quad h0 h1 h2 h3) =
  stepWith (\o0 o1 o2 o3 g0 g1 g2 g3 -> (Quad o0 o1 o2 o3, Quad g0 g1 g2 g3)) a b c d h0 h1 h2 h3

-- | 'step' on the eight words of a state, passed to a continuation so that loops keep the state
-- in registers.
stepWith
  :: (Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> r)
  -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> r
stepWith k a b c d h0 h1 h2 h3 = stepW (narrowed k) (w a) (w b) (w c) (w d) (w h0) (w h1) (w h2) (w h3)
{-# INLINE stepWith #-}

w :: Word32 -> Word
w = fromIntegral
{-# INLINE w #-}

narrowed :: (Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> r) -> Lane r
narrowed k a b c d e f g h =
  k (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d) (fromIntegral e) (fromIntegral f)
    (fromIntegral g) (fromIntegral h)
{-# INLINE narrowed #-}

-- | The seeding function @F@: eight rounds of @T@, a round constant, and a swap of the halves.
seedingF :: Quad -> Quad -> (Quad, Quad)
seedingF (Quad a b c d) (Quad h0 h1 h2 h3) =
  fW (narrowed (\o0 o1 o2 o3 g0 g1 g2 g3 -> (Quad o0 o1 o2 o3, Quad g0 g1 g2 g3))) (w a) (w b) (w c) (w d)
    (w h0) (w h1) (w h2) (w h3)

-- | @F(key, counter, domain, aux)@.
keyedF :: Quad -> Word64 -> Word32 -> Word32 -> (Quad, Quad)
keyedF key counter domain aux =
  keyedFWith (\o0 o1 o2 o3 g0 g1 g2 g3 -> (Quad o0 o1 o2 o3, Quad g0 g1 g2 g3)) key counter domain aux

-- | 'keyedF' passed to a continuation.
keyedFWith
  :: (Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> r)
  -> Quad -> Word64 -> Word32 -> Word32 -> r
keyedFWith k (Quad k0 k1 k2 k3) = keyedFW (narrowed k) k0 k1 k2 k3
{-# INLINE keyedFWith #-}

-- | Block @B(c, j)@: the exposed half of chunk @c@ after @j + 1@ steps.
block :: Quad -> Word64 -> Word32 -> Quad
block key c j = keyedFWith (go (fromIntegral j + 1 :: Int)) key c domainStream auxStream
  where
    go 0 o0 o1 o2 o3 _ _ _ _ = Quad o0 o1 o2 o3
    go n o0 o1 o2 o3 h0 h1 h2 h3 = stepWith (go (n - 1)) o0 o1 o2 o3 h0 h1 h2 h3
