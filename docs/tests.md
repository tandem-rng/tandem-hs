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
- The conformance files of the spec, byte copies in `tests/conformance`, read by
  `tests/Conformance.hs` and run by `tests/Checklist.hs`, which demonstrates every item of the
  spec's `conformance/CHECKLIST.md` at 2a4bd08 that the port offers. Every case of the bounded, normal,
  exponential and weighted choice files runs as a fill, as scalar draws and cut at elements 1,
  7, 20, 21 and n - 1, bit for bit with its end position. The checks cover the choice tables,
  the global draw index of the fallback, the width from the range, empty fills, the pair rule
  and odd `n` of the `Float` normals, rejected weights, the choice law, random access and the
  position bounds. The SHA-256 of every stream and dump in `hashes.json` matches, from the key.
  The port has no `Bool`, 128-bit, binary16, `Char` or complex draws, so those streams and the
  complex block span are left out, and the fill-end check of section 5 cannot be reached with a
  vector that fits in memory.
- The three `Double` normal dumps of `hashes.json` come out equal from the C fill, from scalar
  draws, and from Haskell fills of 1000 elements.
- Bounded and `Double` normal fills cut at any element, across the switch between Haskell and C
  and across the pieces of a long C fill. Empty fills, position rules of draws, `split`, `fork` and `purpose`,
  constructor bounds.
- Four raw moments and the Kolmogorov-Smirnov distance of 10^7 normals and exponentials, in
  `Double` and `Float`.
- `RandomGen` and `SplitGen` against the draws, and the `random` stateful adapters.

## Fixtures

`tools/gen_fixtures.py` converts the specification's `vectors.json` into `tests/Fixtures.hs`.
`tests/conformance/*.json` are byte copies of tandem-spec 2a4bd08 `conformance/*.json`. `tools/gen_zig_tables.py` writes the ziggurat
tables from the spec's JSON. `cabal run -f tools tandem-dump -- normals` writes the bytes of
tandem-c's `tools/dump_normals.c`, with SHA-256
`700ec4d2f4d6b82aaa56c6eff18a4e5919585fdbd093988773383d580ea610d1`.

## CI

CI checks the vendored C, the dumps, the fixtures, the conformance files and the ziggurat tables against the commits
pinned in `.github/workflows/ci.yml`. The tests run with GHC 9.14 on Ubuntu and macOS and with
GHC 9.12 on Ubuntu. CI also checks that the dumps hash as tandem-c's, that fused multiply-adds lower to
instructions, the tests with `-mfma`, and the tests in pure Haskell with `-f -cbits`. A second
job runs the tests with GHC 9.14 and `-f llvm` on LLVM 21, on Ubuntu and macOS.
