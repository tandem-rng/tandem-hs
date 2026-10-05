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

| | Tandem | StdGen | mwc |
|---|---|---|---|
| fill `Word32` | 0.20 | 10.3 | 4.55 |
| fill `Double` | 0.46 | 7.89 | 6.94 |
| fill `Float` | 0.23 | 7.61 | 4.67 |
| fill bounded, range 1000 | 0.50 | 3.79 | 4.46 |
| fill normal `Double` | 0.95 | | 13.6 |
| fill exponential `Double` | 1.28 | | 11.9 |
| fill `Word32`, 2^9 values | 0.96 | | |
| fill `Double`, 2^9 values | 1.89 | | |
| fill `Float`, 2^9 values | 1.12 | | |
| fill normal `Double`, 2^9 values | 5.03 | | |
| scalar `Word64` | 5.15 | 0.76 | 6.28 |
| scalar `Double` | 5.44 | 7.31 | 7.07 |
| scalar normal `Double` | 8.36 | | 13.4 |

The fills of 2^22 values run in the vendored tandem-c, about 19 GiB/s for `Word32`, as fast as
tandem-c itself in the same run. Fills below 1024 values stay in Haskell and reach 4.0 GiB/s, as
GHC's native code generator does not vectorize them. A scalar draw is a pure function of the
generator and returns a new one, so every row costs a new row cache.
