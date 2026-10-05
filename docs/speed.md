# Speed

`cabal bench` produces the figures.

## CPU

One thread on an Apple M4, GHC 9.14.1, `cabal bench`, one run of `tasty-bench`, in ns per
value. Fills write 2^22 values into a new unboxed vector, or 2^9 values into one vector 2^13
times. Scalar rows sum 2^20 draws in a strict
loop. `StdGen` is `random`'s SplitMix generator: its fills are `U.unfoldrExactN` over
`genWord32` and `uniformR`, its bounded draw is `uniformR (0, 999)`, and it has no normals or
exponentials. `mwc` is `mwc-random` 0.15 with `uniformVector`, `uniformR`, `standard` and
`exponential`.

| | Tandem | `-f llvm` | `-f -cbits` | StdGen | mwc |
|---|---|---|---|---|---|
| fill `Word32` | 0.20 | 0.21 | 1.02 | 10.1 | 4.55 |
| fill `Double` | 0.47 | 0.49 | 2.15 | 8.34 | 7.03 |
| fill `Float` | 0.23 | 0.25 | 1.10 | 7.72 | 4.65 |
| fill bounded, range 1000 | 0.51 | 0.55 | 1.79 | 3.72 | 4.74 |
| fill normal `Double` | 1.09 | 1.12 | 5.60 | | 14.7 |
| fill exponential `Double` | 1.30 | 1.34 | 5.29 | | 12.6 |
| fill `Word32`, 2^9 values | 0.24 | 0.26 | 1.08 | | |
| fill `Double`, 2^9 values | 0.51 | 0.54 | 2.14 | | |
| fill `Float`, 2^9 values | 0.27 | 0.29 | 1.30 | | |
| fill normal `Double`, 2^9 values | 1.21 | 1.29 | 6.37 | | |
| scalar `Word64` | 3.04 | 3.36 | 5.83 | 0.90 | 6.64 |
| scalar `Double` | 3.31 | 3.32 | 6.10 | 7.68 | 7.28 |
| scalar normal `Double` | 6.51 | 6.70 | 9.11 | | 15.0 |

Tandem runs on the vendored tandem-c, about 19 GiB/s for `Word32` fills, as fast as tandem-c
itself in the same run, and close to it from 512 values on. The `-f llvm` column builds with LLVM
21, which gains nothing once the rows come from C. The `-f -cbits` column runs every row in
Haskell with the native code generator, which does not vectorize them. A scalar draw is a pure
function of the generator and returns a new one, so it costs about 3 ns where tandem-c's
mutable generator takes 1.5. The `StdGen` and `mwc` columns come from the default build.
