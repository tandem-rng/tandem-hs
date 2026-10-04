{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | The fills of the vendored tandem-c, through @cbits/hs_tandem.c@. Each takes the key words,
-- the position, @K@, the array, the element offset and the length, and returns the end position.
-- The calls are unsafe, so the garbage collector cannot move the array during them.
module System.Random.Tandem.Native
  ( NativeFill
  , NativeBelow
  , cFillU32
  , cFillU64
  , cFillF32
  , cFillF64
  , cFillNormalF32
  , cFillNormalF64
  , cFillExponentialF32
  , cFillExponentialF64
  , cFillU32Below
  , cFillU64Below
  ) where

import Data.Word (Word32, Word64)
import Foreign.C.Types (CSize (..))
import GHC.Exts (MutableByteArray#)

type NativeFill s =
  Word32 -> Word32 -> Word32 -> Word32 -> Word64 -> Word32 -> MutableByteArray# s -> CSize -> CSize -> IO Word64

type NativeBelow s r =
  Word32 -> Word32 -> Word32 -> Word32 -> Word64 -> Word32 -> MutableByteArray# s -> CSize -> CSize -> r -> IO Word64

foreign import ccall unsafe "hs_tandem_fill_u32" cFillU32 :: NativeFill s
foreign import ccall unsafe "hs_tandem_fill_u64" cFillU64 :: NativeFill s
foreign import ccall unsafe "hs_tandem_fill_f32" cFillF32 :: NativeFill s
foreign import ccall unsafe "hs_tandem_fill_f64" cFillF64 :: NativeFill s
foreign import ccall unsafe "hs_tandem_fill_normal_f32" cFillNormalF32 :: NativeFill s
foreign import ccall unsafe "hs_tandem_fill_normal_f64" cFillNormalF64 :: NativeFill s
foreign import ccall unsafe "hs_tandem_fill_exponential_f32" cFillExponentialF32 :: NativeFill s
foreign import ccall unsafe "hs_tandem_fill_exponential_f64" cFillExponentialF64 :: NativeFill s
foreign import ccall unsafe "hs_tandem_fill_u32_below" cFillU32Below :: NativeBelow s Word32
foreign import ccall unsafe "hs_tandem_fill_u64_below" cFillU64Below :: NativeBelow s Word64
