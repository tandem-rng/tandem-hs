<p align="center"><img src="assets/lockup.png" width="560" alt="tandem rng .hs"></p>

# tandem-hs

[![CI](https://github.com/tandem-rng/tandem-hs/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/tandem-rng/tandem-hs/actions/workflows/ci.yml)
[![Docs](https://img.shields.io/badge/docs-tandem--rng.github.io-7fb3ee.svg)](https://tandem-rng.github.io/tandem-hs/)
[![License: Apache 2.0](https://img.shields.io/badge/license-Apache_2.0-blue.svg)](LICENSE)

Haskell implementation of [Tandem8x32](https://github.com/tandem-rng/spec), a noncryptographic
pseudorandom number generator. It produces the stream the specification defines, bit for bit.
Bounded integers, normals, exponentials and weighted choice equal tandem-c's fixtures bit for bit. The stream comes
from a vendored `tandem.c`, or from pure Haskell with the package flag `cbits` off.

It needs GHC 9.12 or newer. `cbits/tandem.c` is vendored from tandem-c `121db59`. Add the
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

`Tandem` is a `RandomGen` and `SplitGen` of `random` 1.3. See [API](docs/api.md) for every
draw and fill, and [design](docs/design.md), [tests](docs/tests.md) and [speed](docs/speed.md)
for the rest.

Portions of the code were generated with the assistance of LLMs.

[Documentation](https://tandem-rng.github.io/tandem-hs/) · [Apache 2.0 license](LICENSE)
