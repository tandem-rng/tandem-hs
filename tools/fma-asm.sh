#!/bin/sh
# Counts the fused multiply-add instructions and the calls of the C library's fma that GHC's
# native code generator emits for the normals and exponentials. Extra arguments go to GHC, such
# as -mfma on x86-64.
#
#   tools/fma-asm.sh [-mfma]
set -eu
out=$(mktemp -d)
cabal build -v0 lib:tandem
cabal exec -v0 -- ghc -O2 -isrc -XGHC2024 -fforce-recomp -ddump-asm -ddump-to-file \
  -dumpdir "$out" -outputdir "$out" "$@" -c \
  src/System/Random/Tandem/Core.hs src/System/Random/Tandem/Native.hs \
  src/System/Random/Tandem/Generator.hs \
  src/System/Random/Tandem/Math.hs src/System/Random/Tandem/ZigTables.hs \
  src/System/Random/Tandem/Derived.hs
asm=$(find "$out" -name 'Derived.dump-asm')
echo "fused $(grep -cE '^[[:space:]]*v?fn?m(add|sub)[0-9a-z]*[[:space:]]' "$asm" || true)"
echo "calls $(grep -cE '^[[:space:]]*(call|callq|bl)[[:space:]].*\bfmaf?\b' "$asm" || true)"
rm -rf "$out"
