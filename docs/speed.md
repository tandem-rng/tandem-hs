# Speed

`cabal bench` prints the rows in GiB/s of output. A scalar draw of `Word64` or `Double` counts
8 bytes.

## CPU

One thread on an Apple M4, GHC 9.14.1. All columns come from one session, one run of each build.
Fills write 2^22 values into a new unboxed vector, or 2^9 values into one vector 2^13 times.
Scalar rows draw 2^20 values in a strict loop.

The last column is the faster of two third-party generators in the default build: `StdGen`,
`random`'s SplitMix generator, and `mwc` from `mwc-random` 0.15. `StdGen` fills are
`U.unfoldrExactN` over `genWord32` and `uniformR`, and its bounded draw is `uniformR (0, 999)`.
It has no normals or exponentials. `mwc` uses `uniformVector`, `uniformR`, `standard` and
`exponential`.

| | Tandem | `-f llvm` | `-f -cbits` | fastest third party |
|---|---|---|---|---|
| fill `Word32` | 18.8 | 18.5 | 4.38 | 0.83 mwc |
| fill `Double` | 15.1 | 16.0 | 3.89 | 1.06 mwc |
| fill `Float` | 16.0 | 15.9 | 3.65 | 0.79 mwc |
| fill bounded, range 1000 | 7.40 | 7.49 | 2.32 | 1.01 StdGen |
| fill normal `Double` | 7.54 | 7.82 | 1.48 | 0.54 mwc |
| fill exponential `Double` | 5.90 | 5.99 | 1.53 | 0.61 mwc |
| fill `Word32`, 2^9 values | 15.4 | 15.7 | 3.86 | |
| fill `Double`, 2^9 values | 14.6 | 15.2 | 3.95 | |
| fill `Float`, 2^9 values | 13.7 | 14.0 | 3.36 | |
| fill normal `Double`, 2^9 values | 6.10 | 6.26 | 1.48 | |
| scalar `Word64` | 2.70 | 2.53 | 1.44 | 9.73 StdGen |
| scalar `Double` | 2.45 | 2.51 | 1.38 | 1.05 mwc |
| scalar normal `Double` | 1.19 | 1.23 | 0.89 | 0.55 mwc |

Tandem runs its long fills on the vendored tandem-c, and the 2^9 fills come close to them. The
`-f llvm` column builds with LLVM 21, which gains little once the rows come from C. The
`-f -cbits` column runs every row in Haskell with the native code generator, which does not
vectorize them. A scalar draw is a pure function of the generator and returns a new one, so
SplitMix, whose state is one word, draws `Word64` faster.
