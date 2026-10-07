# tandem-hs

Haskell implementation of Tandem8x32. It produces the stream of the
[specification](https://github.com/tandem-rng/spec/blob/main/SPEC.md) bit for bit. Bounded
integers, normals and exponentials equal tandem-c's fixtures bit for bit. Fills of 1024 or more
elements run a vendored `tandem.c`, and smaller fills and scalar draws are pure Haskell.

- [API](api.md): the `Tandem` type, its draws and fills, and the `random` instances.
- [Design](design.md): the fills, bounded integers, normals, exponentials and fused
  multiply-adds.
- [Tests](tests.md): what the suite checks, the fixtures, and what CI runs.
- [Speed](speed.md): Apple M4 figures against `StdGen` and `mwc-random`.

## Install

```cabal
-- cabal.project
source-repository-package
  type: git
  location: https://github.com/tandem-rng/tandem-hs
```

GHC 9.12 or newer, `GHC2024`. The library depends on `base`, `primitive`, `vector` and `random`,
and compiles `cbits/tandem.c`, vendored from tandem-c 1c75956, with the C compiler GHC uses.

## AI assistance

This port was written with the help of large language models under human
direction. The design and the specification are human work, as is much of the
Julia implementation. The code is tested bit for bit against every vector of
the specification and against long stream dumps from the Julia implementation,
and every value must match. The output does not depend on who or what wrote the
code.
