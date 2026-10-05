{-# LANGUAGE MagicHash #-}

-- | The arithmetic of the normals and exponentials, copied from tandem-c with the same operation
-- order. Every multiply-add is a fused primop and no plain product feeds a plain sum, so the bits
-- equal tandem-c's on every target. GHC never contracts on its own.
module System.Random.Tandem.Math
  ( neg2Log
  , neg2LogF
  , boxMullerF
  ) where

import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Word (Word32)
import GHC.Exts (Double (D#), Float (F#), fmaddDouble#, fmaddFloat#)
import GHC.Float
  ( castDoubleToWord64
  , castFloatToWord32
  , castWord32ToFloat
  , castWord64ToDouble
  , float2Int
  , int2Double
  , int2Float
  )

fma :: Double -> Double -> Double -> Double
fma (D# x) (D# y) (D# z) = D# (fmaddDouble# x y z)
{-# INLINE fma #-}

fmaf :: Float -> Float -> Float -> Float
fmaf (F# x) (F# y) (F# z) = F# (fmaddFloat# x y z)
{-# INLINE fmaf #-}

-- | @-2 ln x@ for @x@ in @(0, 1]@. With @x = m 2^k@ and @m@ in @[sqrt(1/2), sqrt 2)@, adding the
-- bits of @sqrt(1/2)@ to the exponent field makes the mantissa roll over into it exactly when
-- @m >= sqrt(1/2)@, which picks @k@. Then @-2 ln x = 2 nk ln 2 - 4 s p@ with @s = (m-1)/(m+1)@ and
-- @ln 2@ split so that @nk * ln2_hi@ is exact.
neg2Log :: Double -> Double
neg2Log x =
  let !ix = castDoubleToWord64 x + 0x00095f6200000000
      !nk = int2Double (1023 - fromIntegral (ix `shiftR` 52))
      !mant = castWord64ToDouble ((ix .&. 0x000fffffffffffff) + 0x3fe6a09e00000000)
      !s = (mant - 1) / (mant + 1)
      !zz = s * s
      !p = fma zz (fma zz (fma zz (fma zz (fma zz (fma zz 0.08312363319426472
             0.09070001083303751) 0.11111433317907482) 0.14285712049336274)
             0.2000000000566491) 0.33333333333331017) 1.0
   in fma nk 3.816429394731813e-10 (fma nk 1.3862943607382476 ((s * (-4.0)) * p))
{-# INLINE neg2Log #-}

-- | 'neg2Log' in single precision.
neg2LogF :: Float -> Float
neg2LogF x =
  let !ix = castFloatToWord32 x + 0x004afb0d
      !nk = int2Float (127 - fromIntegral (ix `shiftR` 23))
      !mant = castWord32ToFloat ((ix .&. 0x007fffff) + 0x3f3504f3)
      !s = (mant - 1) / (mant + 1)
      !zz = s * s
      !p = fmaf zz (fmaf zz (fmaf zz 0.14275366 0.20000061) 0.33333334) 1.0
   in fmaf nk 2.857213530660374e-06 (fmaf nk 1.38629150390625 ((s * (-4.0)) * p))
{-# INLINE neg2LogF #-}

-- | The Box-Muller pair @(r cos 2 pi b, r sin 2 pi b)@ with @r = sqrt(-2 ln(1 - a))@. The angle
-- needs no range reduction: @b - q/4@ for the nearest quarter turn @q@ is exact, the series on
-- @[-pi/4, pi/4]@ are short, and a quarter turn is a swap and a sign change on the bits. @2 pi@
-- is a float pair, so that the angle is good to the last bit of the float.
boxMullerF :: Float -> Float -> (Float, Float)
boxMullerF a b =
  let !r = sqrt (neg2LogF (1 - a))
      !q = float2Int (b * 4 + 0.5)
      !f = fmaf (negate (int2Float q)) 0.25 b
      !th = fmaf f (-1.7484555e-7) (f * 6.2831855)
      !w = th * th
      !hs = fmaf w (fmaf w (fmaf w 2.72499e-06 (-0.00019840087)) 0.008333332) (-0.16666667)
      !hc = fmaf w (fmaf w (fmaf w 2.4463761e-05 (-0.0013887589)) 0.04166665) (-0.5)
      !sb = castFloatToWord32 (th * fmaf w hs 1)
      !cb = castFloatToWord32 (fmaf w hc 1)
      -- Odd q swaps the two, bit 1 of q negates the sine, bit 1 of q + 1 negates the cosine.
      !qu = fromIntegral q :: Word32
      !sm = negate (qu .&. 1)
      !xb = ((sb .&. sm) .|. (cb .&. complement sm)) `xor` (((qu + 1) `shiftL` 30) .&. signBit32)
      !yb = ((cb .&. sm) .|. (sb .&. complement sm)) `xor` ((qu `shiftL` 30) .&. signBit32)
   in (r * castWord32ToFloat xb, r * castWord32ToFloat yb)
{-# INLINE boxMullerF #-}

signBit32 :: Word32
signBit32 = 0x80000000
