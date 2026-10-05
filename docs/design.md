# Design

## Fills

The stream comes from the vendored tandem-c. A generator's cache holds a `tandem_rng` and the 32
words of its current row. A scalar draw outside that row loads the row through tandem-c, and
every fill runs in tandem-c, from a copy of the cached `tandem_rng`, so that older generator
values keep theirs. The copy keeps tandem-c's own row cache: a later row of the chunk group costs
one step per row, and only a jump to another group seeds. Normals, exponentials and bounded
draws map the words in Haskell for scalar draws, and in tandem-c for fills.

With the package flag `cbits` off, nothing calls C. The cache then holds the eight lane states of
the current row, rows step in Haskell on `Word` lanes, and every value is the same. CI runs the
tests both ways.

## Bounded integers

Bounded fills follow Appendix A: element `i` takes draw `i`, so a fill consumes exactly one
draw per element, and a rejected draw retries on `split g` of `purpose 0x424c573332` (or
`0x424c573634`) of the fill's key, where `g` is the draw's index in the stream. A fill cut at
any element equals the whole fill. `fillBelow` takes the draw width from the range: 32-bit
draws for a range up to 2^32, 64-bit draws above.

## Normals

`Double` normals use the 1024-layer ziggurat of Appendix A. Element `i` takes 64-bit draw `i`.
A draw outside the inner rectangles, 0.43 % of them, continues on `split g` of
`purpose 0x4e524d3634` of the fill's key, where `g` is the draw's index in the stream. A fill cut
at any element equals the whole fill, and the scalar normal is element 0 of a fill.
`System.Random.Tandem.ZigTables` holds the tables. `tools/gen_zig_tables.py` writes it from the
spec's `tables/normal_f64_zig1024.json` and checks the file's SHA-256.

`Float` normals are Box-Muller. Pair `j` is elements `2j` and `2j + 1` from draws `2j` and
`2j + 1`, the cosine half first. An odd length writes the cosine half of its last pair and still
consumes both draws.

## Exponentials

Exponentials are `-ln(1 - u)` from one draw each.

## Fused multiply-adds

The normals and exponentials copy tandem-c's polynomials with the same operation order. Every
multiply-add is GHC's `fmaddDouble#` or `fmaddFloat#` primop, and GHC never contracts a plain
product and sum, so the values equal tandem-c's bit for bit on every target.

The native code generator lowers the primops without `-fllvm`.

- On aarch64 it emits `fmadd` instructions.
- On x86-64 it emits `vfmadd` instructions with `-mfma`. Without `-mfma` it calls the C library's
  `fma`, which gives the same bits more slowly. The package flag `fma` adds `-mfma`, for CPUs
  with FMA3: `cabal build -f fma`.

The package flag `llvm` compiles through GHC's LLVM backend instead: `cabal build -f llvm`, with
LLVM's `opt` and `llc` on the PATH. LLVM lowers the same primops to fused instructions and
contracts nothing else, as GHC sets no fast-math flags, so the bits do not change.

`tools/fma-asm.sh [-mfma]` counts both in the generated assembly. CI checks both cases. The
vendored `tandem.c` builds with `-ffp-contract=off` and picks its AVX2 and FMA copy at run time on
x86-64.
