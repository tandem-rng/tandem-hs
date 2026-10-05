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
| fill `Word32` | 0.20 | 9.20 | 4.51 |
| fill `Double` | 0.45 | 7.84 | 6.91 |
| fill bounded, range 1000 | 0.49 | 3.43 | 4.39 |
| fill normal `Double` | 0.93 | | 13.4 |
| fill exponential `Double` | 1.23 | | 11.8 |
| fill `Word32`, 2^9 values | 1.41 | | |
| fill `Double`, 2^9 values | 2.79 | | |
| fill normal `Double`, 2^9 values | 6.46 | | |
| scalar `Word64` | 5.83 | 0.75 | 6.17 |
| scalar `Double` | 6.01 | 7.22 | 6.94 |
| scalar normal `Double` | 9.37 | | 13.2 |

The fills of 2^22 values run in the vendored tandem-c, about 19 GiB/s for `Word32`, as fast as
tandem-c itself in the same run. Fills below 1024 values stay in Haskell and reach 2.7 GiB/s, as
GHC's native code generator does not vectorize them. A scalar draw is a pure function of the generator and returns a new one, so
every row costs a new row cache.
