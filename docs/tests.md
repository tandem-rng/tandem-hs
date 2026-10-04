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
  starts, normal pairs in `Double` and `Float`, exponentials. tandem-cuda's bounded, normal and
  exponential fills, exact.
- The FNV-1a hashes of tandem-c's `test_normal_bits.c` and `test_exponential_bits.c`.
- Bounded fills cut at any element, across the switch between Haskell and C and across the
  pieces of a long C fill. Empty fills, position rules of draws, `split`, `fork` and `purpose`,
  constructor bounds.
- Four raw moments and the Kolmogorov-Smirnov distance of 10^7 normals and exponentials, in
  `Double` and `Float`.
- `RandomGen` and `SplitGen` against the draws, and the `random` stateful adapters.

## Fixtures

`tools/gen_fixtures.py` converts the specification's `vectors.json`, tandem-c's headers and
tandem-cuda's headers into `tests/Fixtures.hs`. `cabal run -f tools tandem-dump -- normals`
writes the bytes of tandem-c's `tools/dump_normals.c`, with SHA-256
`cfae418807a7d5f91ecd3e42c33a00943690c6e4b888ee39206738783efe9ded`.

## CI

CI checks the vendored C, the dumps and the fixtures against the commits pinned in
`.github/workflows/ci.yml`. The tests run with GHC 9.14 on Ubuntu and macOS and with GHC 9.12
on Ubuntu. CI also checks that the dumps hash as tandem-c's, that fused multiply-adds lower to
instructions, and the tests with `-mfma`.
