{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | The fills of the vendored tandem-c, through @cbits/hs_tandem.c@, or, with the package flag
-- @cbits@ off, nothing: 'native' is then 'False' and the generator runs in Haskell.
module System.Random.Tandem.Native
  ( native
  , stateWords
  , Kind (..)
  , cRun
  ) where

import Data.Word (Word32, Word64)
import Foreign.C.Types (CInt (..), CSize (..))
import GHC.Exts (MutableByteArray#)

-- | The element kinds of @hs_tandem_run@, in its order.
data Kind
  = KU32
  | KU64
  | KF32
  | KF64
  | KNormalF32
  | KNormalF64
  | KExponentialF32
  | KExponentialF64
  | KU32Below
  | KU64Below
  deriving (Enum)

-- | Whether the generator runs on tandem-c.
native :: Bool

-- | The size of a tandem_rng in 32-bit words, rounded up to a multiple of 2.
stateWords :: Int

#ifdef TANDEM_CBITS
native = True

-- | @cRun st fresh k0 k1 k2 k3 kk pos kind out off n range@ fills @n@ elements of @kind@ at
-- element offset @off@ of @out@ from bit position @pos@, continuing the tandem_rng in @st@, or
-- starting it from the transport form when @fresh@ is nonzero, and returns the end position. The
-- call is unsafe, so the garbage collector cannot move the arrays during it.
foreign import ccall unsafe "hs_tandem_run" cRun
  :: MutableByteArray# s -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word64 -> CInt
  -> MutableByteArray# s -> CSize -> CSize -> Word64 -> IO Word64

foreign import ccall unsafe "hs_tandem_state_bytes" stateBytes :: CSize

stateWords = 2 * ((fromIntegral stateBytes + 7) `quot` 8)
#else
native = False

cRun
  :: MutableByteArray# s -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word64 -> CInt
  -> MutableByteArray# s -> CSize -> CSize -> Word64 -> IO Word64
cRun _ _ _ _ _ _ _ _ _ _ _ _ _ = error "System.Random.Tandem: built without cbits"

stateWords = 0
#endif
{-# INLINE native #-}
