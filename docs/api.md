# API

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

## Reference

- `Tandem`: a pure value. It is the transport form, a 128-bit key, a 64-bit bit position and the
  chunk length `K`, plus a cache of the current row. `Eq` and `Show` cover the transport form.
- `seed`, `fromKey`, `key`, `position`, `seek`, `chunkLength`.
- `split`, `fork`, `purpose`: child generators by index, from the current block, and by name.
- Scalar draws that return the value and the advanced generator: `nextWord32`, `nextWord64`,
  `nextDouble`, `nextFloat`, `nextBelow32`, `nextBelow64`, `nextNormal`, `nextNormalFloat`,
  `nextNormalPairFloat`, `nextExponential`, `nextExponentialFloat`, `nextChoice`.
- Fills into new unboxed vectors (`fillWord32 n g`) and in place into `Data.Vector.Unboxed`
  mutable vectors or slices of them (`fillWord32M v g`), for every kind above:
  `fillWord32`, `fillWord64`, `fillDouble`, `fillFloat`, `fillBelow32`, `fillBelow64`,
  `fillBelow`, `fillNormal`, `fillNormalFloat`, `fillExponential`, `fillExponentialFloat`,
  `fillChoice`.
- Weighted choice, Appendix C of the specification: `choice weights` builds the alias table of a
  `Vector Double` in exact integers, or returns `Nothing` unless there are 1 to 2^32 - 1 weights,
  all finite and not negative, and one is positive. `nextChoice` and `fillChoice` return
  zero-based `Word32` indices, one 64-bit draw each with no retry, so a fill cut anywhere equals
  the whole fill. An empty fill aligns the position to 64 bits. `choiceSize`, `choiceCapacity`,
  `choiceCuts` and `choiceAliases` expose the table. The table and the draw run in Haskell, over
  the C or the Haskell 64-bit fill.
- Every draw and fill aligns the position to its width, as the specification requires. A plain
  fill or a `Double` normal fill of 0 elements aligns the position. A bounded, `Float` normal
  or exponential fill of 0 elements leaves it as it is.
- `System.Random.Tandem.Core`: the step `T`, the seeding function `F` and the blocks.

## random

`Tandem` is a `RandomGen` and a `SplitGen` of `random` 1.3, so `random`'s adapters make it a
`StatefulGen`:

```haskell
import System.Random.Stateful

roll :: IO Word32
roll = do
  gen <- newIOGenM (T.seed 42)
  uniformRM (1, 6) gen
```

`genWord8`, `genWord16`, `genWord32` and `genWord64` draw their widths from the stream.
`genWord32R` and `genWord64R` use Lemire's method with the width from the range, so
`genWord64R` with a range up to 2^32 equals `genWord32R`. `splitGen` is `fork 1`.

## Parallel use

Element `i` of a fill is draw `i`, so any decomposition reproduces a serial run. See
[Appendix B](https://github.com/tandem-rng/spec/blob/main/SPEC.md#appendix-b-parallel-decomposition-non-normative).
