# Speed

`cabal bench` produces the figures.

## CPU

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
| fill normal `Double` | 0.97 | | 13.4 |
| fill exponential `Double` | 1.29 | | 11.9 |
| scalar `Word64` | 6.16 | 0.77 | 6.38 |
| scalar `Double` | 6.28 | 7.29 | 7.13 |
| scalar normal `Double` | 12.4 | | 13.3 |

These fills run in the vendored tandem-c, about 17 GiB/s for `Word32` against 20.4 GiB/s for
tandem-c itself. The Haskell fills reach 2.7 GiB/s, as GHC's native code generator does not
vectorize them. A scalar draw is a pure function of the generator and returns a new one, so
every row costs a new row cache.
