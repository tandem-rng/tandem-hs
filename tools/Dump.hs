-- | Writes the normal or exponential fills of tandem-c's tools/dump_normals.c and
-- tools/dump_exponentials.c to stdout as the same raw little-endian bytes:
--
-- > cabal run -f tools tandem-dump -- normals | shasum -a 256
module Main (main) where

import Data.ByteString.Builder (Builder, doubleLE, floatLE, hPutBuilder)
import Data.Vector.Unboxed qualified as U
import System.Environment (getArgs)
import System.IO (hSetBinaryMode, stdout)
import System.Random.Tandem

main :: IO ()
main = do
  args <- getArgs
  hSetBinaryMode stdout True
  case args of
    ["normals"] -> dump (\g -> bytes doubleLE (fst (fillNormal 1000000 g)))
    ["exponentials"] ->
      dump $ \g ->
        let (d, g') = fillExponential 1000000 g
         in bytes doubleLE d <> bytes floatLE (fst (fillExponentialFloat 1000000 g'))
    _ -> fail "usage: tandem-dump normals|exponentials"
  where
    dump fills = mapM_ (\p -> hPutBuilder stdout (fills (seek p (seed (2026 + 7 * 2 ^ (64 :: Int))))))
      [0, 1, 77, 12345, 2 ^ (30 :: Int)]

bytes :: U.Unbox a => (a -> Builder) -> U.Vector a -> Builder
bytes put = U.foldr (\x b -> put x <> b) mempty
