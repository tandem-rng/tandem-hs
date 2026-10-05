/* Entry points for System.Random.Tandem.Native. The Haskell generator keeps a tandem_rng in its
 * row cache and copies it before each call, so that older generator values keep theirs. */
#include <string.h>

#include "tandem.h"

size_t hs_tandem_state_bytes(void) { return sizeof(tandem_rng); }

enum { U32, U64, F32, F64, NORMAL_F32, NORMAL_F64, EXPONENTIAL_F32, EXPONENTIAL_F64, U32_BELOW, U64_BELOW };

/* Fill n elements of the given kind at element offset off of out, from bit position pos, and
 * return the end position. A fresh state starts from the transport form. Otherwise the call sets
 * the position field alone and keeps the state's row cache, which tandem.c checks against the
 * row of every read: a read in the cached row reuses it, a later row of its chunk group steps it
 * forward, and any other row seeds. tandem_set_position would drop the cache and seed each time. */
uint64_t hs_tandem_run(tandem_rng *st, int fresh, uint32_t k0, uint32_t k1, uint32_t k2, uint32_t k3,
                       uint32_t K, uint64_t pos, int kind, void *out, size_t off, size_t n,
                       uint64_t range) {
    if (fresh) {
        const uint32_t key[4] = {k0, k1, k2, k3};
        *st = tandem_from_key(key, pos, K);
    } else {
        st->pos = pos;
    }
    switch (kind) {
    case U32: tandem_fill_u32(st, (uint32_t *)out + off, n); break;
    case U64: tandem_fill_u64(st, (uint64_t *)out + off, n); break;
    case F32: tandem_fill_f32(st, (float *)out + off, n); break;
    case F64: tandem_fill_f64(st, (double *)out + off, n); break;
    case NORMAL_F32: tandem_fill_normal_f32(st, (float *)out + off, n); break;
    case NORMAL_F64: tandem_fill_normal_f64(st, (double *)out + off, n); break;
    case EXPONENTIAL_F32: tandem_fill_exponential_f32(st, (float *)out + off, n); break;
    case EXPONENTIAL_F64: tandem_fill_exponential_f64(st, (double *)out + off, n); break;
    case U32_BELOW: tandem_fill_u32_below(st, (uint32_t *)out + off, n, (uint32_t)range); break;
    case U64_BELOW: tandem_fill_u64_below(st, (uint64_t *)out + off, n, range); break;
    }
    return tandem_position(st);
}
