# Tests

```sh
cabal test
```

## Suite

- The specification vectors, the step, the seeding function, blocks, derived keys and seed
  whitening.
- Fills, scalar draws and fills cut into slices against the stream dumps in `tests/data`, at
  `K = 8` and `K = 32`, and `genWord8` against the byte dump. Random access at chunk lengths 1
  to 65536 against the definition.
- tandem-c's cross fixtures, exact: scalar bounded draws, bounded fills at aligned and unaligned
  starts, `Float` normal pairs, exponentials. The six `Double` ziggurat rows of
  `cross_normal.h`, whose misses cover the wedge and the tail, from the C fill, the Haskell
  fill, scalar draws and the Haskell fill cut at every element. tandem-cuda's bounded, normal and
  exponential fills, exact.
- The FNV-1a hashes of tandem-c's `test_normal_bits.c` and `test_exponential_bits.c`. The
  `Double` normal hashes, `a61cfa844c85f7c1` and the two of the Python reference, come out
  equal from the C fill, from scalar draws, and from Haskell fills of 1000 elements.
- Bounded and `Double` normal fills cut at any element, across the switch between Haskell and C
  and across the pieces of a long C fill. Empty fills, position rules of draws, `split`, `fork` and `purpose`,
  constructor bounds.
- Four raw moments and the Kolmogorov-Smirnov distance of 10^7 normals and exponentials, in
  `Double` and `Float`.
- `RandomGen` and `SplitGen` against the draws, and the `random` stateful adapters.

## Fixtures

`tools/gen_fixtures.py` converts the specification's `vectors.json`, tandem-c's headers and
tandem-cuda's headers into `tests/Fixtures.hs`. `tools/gen_zig_tables.py` writes the ziggurat
tables from the spec's JSON. `cabal run -f tools tandem-dump -- normals` writes the bytes of
tandem-c's `tools/dump_normals.c`, with SHA-256
`700ec4d2f4d6b82aaa56c6eff18a4e5919585fdbd093988773383d580ea610d1`.

## CI

CI checks the vendored C, the dumps, the fixtures and the ziggurat tables against the commits
pinned in `.github/workflows/ci.yml`. The tests run with GHC 9.14 on Ubuntu and macOS and with
GHC 9.12 on Ubuntu. CI also checks that the dumps hash as tandem-c's, that fused multiply-adds lower to
instructions, the tests with `-mfma`, and the tests in pure Haskell with `-f -cbits`. A second
job runs the tests with GHC 9.14 and `-f llvm` on LLVM 21, on Ubuntu and macOS.
