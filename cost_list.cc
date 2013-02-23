#include "powcosts/cost_list.h"

#include <assert.h>

extern "C" {
#include "libqform/dbreps/s128_pow_reps.h"
#include "libqform/dbreps/s64_pow_reps.h"
#include "libqform/s128_qform.h"
#include "libqform/s64_qform.h"
}

double CostList::cost_u16(const group_cost_t& cost,
			  const uint16_t n) const {
  const factored_two_three_term16_t* const* rep_terms;
  const int* rep_sizes;
  if (&cost == &s64_qform_costs) {
    rep_terms = s64_pow_reps;
    rep_sizes = s64_pow_rep_sizes;
  } else if (&cost == &s128_qform_costs) {
    rep_terms = s128_pow_reps;
    rep_sizes = s128_pow_rep_sizes;
  } else {
    printf("Unrecognized costs.\n");
    exit(-1);
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

double CostList::cost(const group_cost_t& cost,
		      const mpz_c& in_n) const {
  mpz_c n(in_n);
  int d = 2;
  double res = 0;
  while (mpz_cmp_ui(n.z, 1) != 0) {
    if (mpz_divisible_ui_p(n.z, d)) {
      res += cost_u16(cost, d);
      mpz_divexact_ui(n.z, n.z, d);
    } else {
      d++;
      if (d >= std::numeric_limits<uint16_t>::max()) {
	printf("CostList::cost - input exponent has prime divisor larger than 16-bits!\n");
	exit(-1);
      }
    }
  }
  return res;
}
