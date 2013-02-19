#include "powcosts/cost_block.h"

#include <assert.h>

extern "C" {
#include "libqform/dbreps/s128_pow_reps.h"
#include "libqform/dbreps/s64_pow_reps.h"
#include "libqform/s128_qform.h"
#include "libqform/s64_qform.h"
}

double CostBlock::cost_u16(const group_cost_t& cost,
			   const uint16_t n) const {
  const factored_two_three_term16_t* const* rep_terms;
  const int* rep_sizes;
  if (&cost == &s64_qform_costs) {
    rep_terms = s64_pow_reps;
    rep_sizes = s64_pow_rep_sizes;
  } else {
    rep_terms = s128_pow_reps;
    rep_sizes = s128_pow_rep_sizes;
  }
  int a = 0;
  int b = 0;
  const int j = rep_sizes[n];
  const factored_two_three_term16_t* term = rep_terms[n];
  for (int i = 0; i < j; i++, term++) {
    a += term->a;
    int t = term->b & ~(1<<15);
    if (t > b) b = t;
  }
  return (j - 1) * cost.compose + a * cost.square + b * cost.cube;
}

double CostBlock::cost(const group_cost_t& cost,
		       const mpz_c& in_n) const {
  int limbs = abs(in_n.z->_mp_size);
  const uint16_t* p = reinterpret_cast<const uint16_t*>(in_n.z->_mp_d);
  p += limbs * (GMP_LIMB_BITS / 16) - 1;
  double res = 0;
  while (p >= reinterpret_cast<const uint16_t*>(in_n.z->_mp_d)) {
    res += cost_u16(cost, *p);
    p--;
  }
  return res;
}
