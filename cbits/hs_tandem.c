/* Entry points for System.Random.Tandem.Native. Haskell passes the transport form and an element
 * offset into its array, and gets the end position back. The row cache stays on the Haskell side. */
#include "tandem.h"

#define FILL(name, type, call)                                                                    \
    uint64_t hs_tandem_##name(uint32_t k0, uint32_t k1, uint32_t k2, uint32_t k3, uint64_t pos,  \
                              uint32_t K, type *out, size_t off, size_t n) {                     \
        const uint32_t key[4] = {k0, k1, k2, k3};                                               \
        tandem_rng rng = tandem_from_key(key, pos, K);                                           \
        call(&rng, out + off, n);                                                                \
        return tandem_position(&rng);                                                            \
    }

FILL(fill_u32, uint32_t, tandem_fill_u32)
FILL(fill_u64, uint64_t, tandem_fill_u64)
FILL(fill_f32, float, tandem_fill_f32)
FILL(fill_f64, double, tandem_fill_f64)
FILL(fill_normal_f32, float, tandem_fill_normal_f32)
FILL(fill_normal_f64, double, tandem_fill_normal_f64)
FILL(fill_exponential_f32, float, tandem_fill_exponential_f32)
FILL(fill_exponential_f64, double, tandem_fill_exponential_f64)

#define FILL_BELOW(name, type, call)                                                              \
    uint64_t hs_tandem_##name(uint32_t k0, uint32_t k1, uint32_t k2, uint32_t k3, uint64_t pos,  \
                              uint32_t K, type *out, size_t off, size_t n, type range) {         \
        const uint32_t key[4] = {k0, k1, k2, k3};                                               \
        tandem_rng rng = tandem_from_key(key, pos, K);                                           \
        call(&rng, out + off, n, range);                                                         \
        return tandem_position(&rng);                                                            \
    }

FILL_BELOW(fill_u32_below, uint32_t, tandem_fill_u32_below)
FILL_BELOW(fill_u64_below, uint64_t, tandem_fill_u64_below)
