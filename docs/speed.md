# Speed

`cabal bench` prints the rows: fills in GiB/s of output, scalar draws in ns per draw.

## CPU

One thread on an Apple M4, GHC 9.14.1, one run of each build. Fills write 2^22 values into a new
unboxed vector, or 2^9 values into one vector 2^13 times. Scalar rows sum 2^20 draws in a strict
loop. `StdGen` is `random`'s SplitMix generator: its fills are `U.unfoldrExactN` over
`genWord32` and `uniformR`, its bounded draw is `uniformR (0, 999)`, and it has no normals or
exponentials. `mwc` is `mwc-random` 0.15 with `uniformVector`, `uniformR`, `standard` and
`exponential`. The `StdGen` and `mwc` columns come from the default build.

### Fills, GiB/s of output

| | Tandem | `-f llvm` | `-f -cbits` | StdGen | mwc |
|---|---|---|---|---|---|
| fill `Word32` | 18.8 | 15.4 | 3.47 | 0.40 | 0.82 |
| fill `Double` | 16.3 | 13.3 | 3.07 | 0.95 | 1.05 |
| fill `Float` | 16.2 | 13.3 | 2.92 | 0.49 | 0.79 |
| fill bounded, range 1000 | 7.47 | 5.92 | 1.85 | 1.00 | 0.84 |
| fill normal `Double` | 7.65 | 6.06 | 1.35 | | 0.52 |
| fill exponential `Double` | 5.88 | 4.91 | 1.48 | | 0.61 |
| fill `Word32`, 2^9 values | 14.6 | 12.1 | 3.85 | | |
| fill `Double`, 2^9 values | 14.7 | 11.9 | 3.89 | | |
| fill `Float`, 2^9 values | 12.3 | 10.9 | 3.26 | | |
| fill normal `Double`, 2^9 values | 5.47 | 4.92 | 1.48 | | |

### Scalar draws, ns per draw

| | Tandem | `-f llvm` | `-f -cbits` | StdGen | mwc |
|---|---|---|---|---|---|
| scalar `Word64` | 3.55 | 4.31 | 5.18 | 0.85 | 7.07 |
| scalar `Double` | 4.00 | 4.41 | 5.47 | 8.84 | 8.79 |
| scalar normal `Double` | 8.53 | 8.59 | 8.29 | | 16.5 |

Tandem runs its long fills on the vendored tandem-c, and the 2^9 fills come close to them. The
`-f llvm` column builds with LLVM 21, which gains nothing once the rows come from C. The
`-f -cbits` column runs every row in Haskell with the native code generator, which does not
vectorize them. A scalar draw is a pure function of the generator and returns a new one, so it
costs more than a draw from a mutable generator.
