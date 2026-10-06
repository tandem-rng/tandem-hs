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
`exponential`. Their 2^9 fills write one draw per value into one vector, as Tandem's do.

| | Tandem | `-f llvm` | `-f -cbits` | fastest third party |
|---|---|---|---|---|
| fill `Word32` | 18.3 | 18.8 | 4.33 | 0.83 mwc |
| fill `Double` | 16.2 | 16.2 | 3.77 | 1.06 mwc |
| fill `Float` | 16.1 | 16.0 | 3.61 | 0.80 mwc |
| fill bounded, range 1000 | 7.53 | 7.40 | 2.27 | 1.01 StdGen |
| fill normal `Double` | 7.63 | 7.80 | 1.44 | 0.54 mwc |
| fill exponential `Double` | 5.98 | 5.99 | 1.49 | 0.63 mwc |
| fill `Word32`, 2^9 values | 15.8 | 15.8 | 3.75 | 1.63 StdGen |
| fill `Double`, 2^9 values | 15.0 | 15.1 | 3.84 | 1.08 mwc |
| fill `Float`, 2^9 values | 13.8 | 14.1 | 3.22 | 0.90 mwc |
| fill normal `Double`, 2^9 values | 6.25 | 6.21 | 1.45 | 0.55 mwc |
| scalar `Word64` | 2.71 | 2.49 | 1.39 | 9.86 StdGen |
| scalar `Double` | 2.43 | 2.50 | 1.31 | 1.05 mwc |
| scalar normal `Double` | 1.23 | 1.24 | 0.89 | 0.56 mwc |

Tandem runs its long fills on the vendored tandem-c, and the 2^9 fills come close to them. The
`-f llvm` column builds with LLVM 21, which gains little once the rows come from C. The
`-f -cbits` column runs every row in Haskell with the native code generator, which does not
vectorize them. A scalar draw is a pure function of the generator and returns a new one, so
SplitMix, whose state is one word, draws `Word64` faster.
