<p align="center"><img src="assets/lockup.png" width="560" alt="tandem rng .hs"></p>

# tandem-hs

[![CI](https://github.com/tandem-rng/tandem-hs/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/tandem-rng/tandem-hs/actions/workflows/ci.yml)
[![License: Apache 2.0](https://img.shields.io/badge/license-Apache_2.0-blue.svg)](LICENSE)

Haskell implementation of [Tandem8x32](https://github.com/tandem-rng/spec), a noncryptographic
pseudorandom number generator. It produces the stream the specification defines, bit for bit.
Bounded integers, normals and exponentials equal tandem-c's fixtures bit for bit. Fills of 1024
or more elements run a vendored `tandem.c`, and smaller fills and scalar draws are pure Haskell.

It needs GHC 9.12 or newer. `cbits/tandem.c` is vendored from tandem-c `4e9a69f`. Add the
repository to `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/tandem-rng/tandem-hs
```

```haskell
import System.Random.Tandem qualified as T

example = (x, zs, k)
  where
    g0 = T.seed 42                       -- 128-bit seed, K = 32
    (x, g1) = T.nextDouble g0            -- uniform in [0, 1)
    (zs, g2) = T.fillNormal 1000 g1      -- unboxed vector of standard normals
    worker = T.split 7 g2                -- by index, from the key alone
    (k, _) = T.nextBelow32 10 worker     -- uniform in [0, 10), Lemire
```

`Tandem` is a `RandomGen` and `SplitGen` of `random` 1.3. See [docs/notes.md](docs/notes.md)
for the API, tests and speed.

Portions of the code were generated with the assistance of LLMs.

[Documentation](docs/notes.md) · [Apache 2.0 license](LICENSE)
