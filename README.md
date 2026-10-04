# tandem-hs

Haskell implementation of [Tandem8x32](https://github.com/tandem-rng/spec), a noncryptographic
pseudorandom number generator fast on CPUs and GPUs alike. The package is `tandem`. It produces
the stream the specification defines, bit for bit, and its bounded integers, normals and
exponentials equal those of tandem-c bit for bit.

## Install

```cabal
-- cabal.project
source-repository-package
  type: git
  location: https://github.com/tandem-rng/tandem-hs
```

GHC 9.12 or newer, `GHC2024`. The library depends on `base`, `primitive`, `vector` and `random`,
and compiles `cbits/tandem.c`, vendored from tandem-c 4e9a69f, with the C compiler GHC uses.

## Use

```haskell
import Data.Vector.Unboxed qualified as U
import Data.Word (Word32)
import System.Random.Tandem qualified as T

example :: (Double, Word32, T.Tandem)
example =
  let g0 = T.seed 42                       -- 128-bit seed, K = 32
      (x, g1) = T.nextDouble g0            -- uniform in [0, 1)
      (k, g2) = T.nextBelow32 10 g1        -- uniform in [0, 10), Lemire
      (zs, g3) = T.fillNormal 1000 g2      -- unboxed vector of standard normals
      worker = T.split 7 g3                -- by index, from the key alone
      (kids, g4) = T.fork 4 g3             -- from the current block, the parent moves on
   in (x + U.sum zs, k, g4)
```

`Tandem` is a `RandomGen` and a `SplitGen` of `random` 1.3, so `random`'s adapters make it a
`StatefulGen`:

```haskell
import System.Random.Stateful

roll :: IO Word32
roll = do
  gen <- newIOGenM (T.seed 42)
  uniformRM (1, 6) gen
```

## What it provides

- `Tandem`: a pure value. It is the transport form, a 128-bit key, a 64-bit bit position and the
  chunk length `K`, plus a cache of the current row. `Eq` and `Show` cover the transport form.
- `seed`, `fromKey`, `key`, `position`, `seek`, `chunkLength`.
- `split`, `fork`, `purpose`: child generators by index, from the current block, and by name.
- Scalar draws that return the value and the advanced generator: `nextWord32`, `nextWord64`,
  `nextDouble`, `nextFloat`, `nextBelow32`, `nextBelow64`, `nextNormal`, `nextNormalFloat`,
  `nextNormalPair`, `nextNormalPairFloat`, `nextExponential`, `nextExponentialFloat`.
- Fills into new unboxed vectors (`fillWord32 n g`) and in place into `Data.Vector.Unboxed`
  mutable vectors or slices of them (`fillWord32M v g`), for every kind above:
  `fillWord32`, `fillWord64`, `fillDouble`, `fillFloat`, `fillBelow32`, `fillBelow64`,
  `fillBelow`, `fillNormal`, `fillNormalFloat`, `fillExponential`, `fillExponentialFloat`.
- Every draw and fill aligns the position to its width, as the specification requires. A plain
  fill of 0 elements aligns the position. A bounded, normal or exponential fill of 0 elements
  leaves it as it is.
- Bounded fills follow Appendix A: element `i` takes draw `i`, so a fill consumes exactly one
  draw per element, and a rejected draw retries on `split g` of `purpose 0x424c573332` (or
  `0x424c573634`) of the fill's key, where `g` is the draw's index in the stream. A fill cut at
  any element equals the whole fill. `fillBelow` takes the draw width from the range: 32-bit
  draws for a range up to 2^32, 64-bit draws above.
- Normals: Box-Muller, pair `j` is elements `2j` and `2j + 1` from draws `2j` and `2j + 1`, the
  cosine half first. An odd length writes the cosine half of its last pair and still consumes
  both draws. The scalar normal is element 0 of a fill.
- Exponentials: `-ln(1 - u)` from one draw each.
- `RandomGen`: `genWord8`, `genWord16`, `genWord32` and `genWord64` draw their widths from the
  stream. `genWord32R` and `genWord64R` use Lemire's method with the width from the range, so
  `genWord64R` with a range up to 2^32 equals `genWord32R`. `splitGen` is `fork 1`.
- `System.Random.Tandem.Core`: the step `T`, the seeding function `F` and the blocks.
- Fills of 1024 or more elements call the vendored tandem-c. Shorter fills and scalar draws run
  in Haskell. Both give the same values.
- Parallel use: element `i` of a fill is draw `i`, so any decomposition reproduces a serial run.
  See [Appendix B](https://github.com/tandem-rng/spec/blob/main/SPEC.md#appendix-b-parallel-decomposition-non-normative).

### Fused multiply-adds

The normals and exponentials copy tandem-c's polynomials with the same operation order. Every
multiply-add is GHC's `fmaddDouble#` or `fmaddFloat#` primop, and GHC never contracts a plain
product and sum, so the values equal tandem-c's bit for bit on every target.

The native code generator lowers the primops without `-fllvm`.

- On aarch64 it emits `fmadd` instructions.
- On x86-64 it emits `vfmadd` instructions with `-mfma`. Without `-mfma` it calls the C library's
  `fma`, which gives the same bits more slowly. The package flag `fma` adds `-mfma`, for CPUs
  with FMA3: `cabal build -f fma`.

`tools/fma-asm.sh [-mfma]` counts both in the generated assembly. CI checks both cases. The
vendored `tandem.c` builds with `-ffp-contract=off` and picks its AVX2 and FMA copy at run time on
x86-64.

## Tests

```sh
cabal test
```

- The specification vectors, the step, the seeding function, blocks, derived keys and seed
  whitening.
- Fills, scalar draws and fills cut into slices against the stream dumps in `tests/data`, at
  `K = 8` and `K = 32`, and `genWord8` against the byte dump. Random access at chunk lengths 1
  to 65536 against the definition.
- tandem-c's cross fixtures, exact: scalar bounded draws, bounded fills at aligned and unaligned
  starts, normal pairs in `Double` and `Float`, exponentials. tandem-cuda's bounded, normal and
  exponential fills, exact.
- The FNV-1a hashes of tandem-c's `test_normal_bits.c` and `test_exponential_bits.c`.
- Bounded fills cut at any element, across the switch between Haskell and C and across the
  pieces of a long C fill. Empty fills, position rules of draws, `split`, `fork` and `purpose`,
  constructor bounds.
- Four raw moments and the Kolmogorov-Smirnov distance of 10^7 normals and exponentials, in
  `Double` and `Float`.
- `RandomGen` and `SplitGen` against the draws, and the `random` stateful adapters.

`tools/gen_fixtures.py` converts the specification's `vectors.json`, tandem-c's headers and
tandem-cuda's headers into `tests/Fixtures.hs`. CI checks the vendored C, the dumps and the
fixtures against the commits pinned in `.github/workflows/ci.yml`. `cabal run -f tools tandem-dump -- normals` writes the bytes of tandem-c's
`tools/dump_normals.c`, with SHA-256
`cfae418807a7d5f91ecd3e42c33a00943690c6e4b888ee39206738783efe9ded`.

## Speed

One thread on an Apple M4, GHC 9.14.1, `cabal bench`, one run of `tasty-bench`, in ns per
value. Fills write 2^22 values into a new unboxed vector. Scalar rows sum 2^20 draws in a strict
loop. `StdGen` is `random`'s SplitMix generator: its fills are `U.unfoldrExactN` over
`genWord32` and `uniformR`, its bounded draw is `uniformR (0, 999)`, and it has no normals or
exponentials. `mwc` is `mwc-random` 0.15 with `uniformVector`, `uniformR`, `standard` and
`exponential`.

| | Tandem | StdGen | mwc |
|---|---|---|---|
| fill `Word32` | 0.21 | 9.11 | 4.51 |
| fill `Double` | 0.49 | 7.84 | 7.03 |
| fill bounded, range 1000 | 0.51 | 3.50 | 4.39 |
| fill normal `Double` | 1.55 | | 13.5 |
| fill exponential `Double` | 1.29 | | 11.9 |
| scalar `Word64` | 6.16 | 0.77 | 6.38 |
| scalar `Double` | 6.28 | 7.29 | 7.13 |
| scalar normal `Double` | 19.7 | | 13.3 |

These fills run in the vendored tandem-c, about 17 GiB/s for `Word32` against 20.4 GiB/s for
tandem-c itself. The Haskell fills reach 2.7 GiB/s, as GHC's native code generator does not
vectorize them. A scalar draw is a pure function of the generator and returns a new one, so
every row costs a new row cache.

## AI assistance

This port was written with the help of large language models under human
direction. The design and the specification are human work, as is much of the
Julia implementation. The code is tested bit for bit against every vector of
the specification and against long stream dumps from the Julia implementation,
and every value must match. The output does not depend on who or what wrote the
code.

## License

Apache License 2.0. See `LICENSE` and `NOTICE`.
