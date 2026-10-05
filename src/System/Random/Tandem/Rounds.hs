-- | The step @T@ and the seeding function @F@ on 'Word' values below @2^32@, for the row loops.
-- GHC's native code generator narrows and masks after every 'Word32' operation, so the loops
-- keep each 32-bit word in a 'Word' and mask only where a result can reach @2^32@: products,
-- left shifts and the Weyl addition.
module System.Random.Tandem.Rounds
  ( Lane
  , stepW
  , fW
  , keyedFW
  ) where

import Data.Bits (unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.))
import Data.Word (Word32, Word64)

-- | A continuation on the eight words of a state, @o@ then @h@.
type Lane r = Word -> Word -> Word -> Word -> Word -> Word -> Word -> Word -> r

m32 :: Word
m32 = 0xffffffff

rotl :: Word -> Int -> Word
rotl x k = ((x `unsafeShiftL` k) .|. (x `unsafeShiftR` (32 - k))) .&. m32
{-# INLINE rotl #-}

-- | The step @T@ on the state @(o, h)@.
stepW :: Lane r -> Lane r
stepW k !a !b !c !d !h0 !h1 !h2 !h3 =
  let !p0 = a * (h0 .|. 1)
      !p1 = c * (h1 .|. 1)
      !l0 = p0 .&. m32
      !l1 = p1 .&. m32
      !n0 = b `xor` (p1 `unsafeShiftR` 32) `xor` l1
      !n1 = rotl l1 16 `xor` h2
      !n2 = d `xor` (p0 `unsafeShiftR` 32) `xor` l0
      !n3 = rotl l0 16 `xor` h3
      !g0 = h0 `xor` rotl h1 7
      !g1 = h1 `xor` rotl h2 13
      !g2 = h2 `xor` rotl h3 22
      !g3 = h3 `xor` rotl g0 3
   in k n0 n1 n2 n3 (((g0 + 0x9e3779b9) .&. m32) `xor` n0) g1 g2 g3
{-# INLINE stepW #-}

-- | The seeding function @F@: eight rounds of @T@, a round constant, and a swap of the halves.
fW :: Lane r -> Lane r
fW k = rnd 0xd17cc1b7 (rnd 0xa7220a94 (rnd 0xfe13abe8 (rnd 0xfa9a6ee0
          (rnd 0xedb14acc (rnd 0x9e21c820 (rnd 0xff28b1d5 (rnd 0xef5de2b0 k)))))))
  where
    rnd rc next = stepW (\o0 o1 o2 o3 g0 g1 g2 g3 -> next g0 g1 g2 g3 (o0 `xor` rc) o1 o2 o3)
    {-# INLINE rnd #-}
{-# INLINE fW #-}

-- | @F(key, counter, domain, aux)@ on the key words @k0@ to @k3@.
keyedFW :: Lane r -> Word32 -> Word32 -> Word32 -> Word32 -> Word64 -> Word32 -> Word32 -> r
keyedFW k k0 k1 k2 k3 counter domain aux =
  fW k (fromIntegral counter .&. m32) (fromIntegral (counter `unsafeShiftR` 32)) (fromIntegral domain)
    (fromIntegral aux) (fromIntegral k0) (fromIntegral k1) (fromIntegral k2) (fromIntegral k3)
{-# INLINE keyedFW #-}
