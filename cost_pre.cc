#include "powcosts/cost_pre.h"

#include <assert.h>

double CostPre::cost(const group_cost_t& cost, const mpz_c& in_n) const {
  long n = mpz_get_ui(in_n.z);
  assert(n >= 0 && n <= 65535);

  int a = 0;
  int b = 0;
  const int j = rep_sizes_[n];
  const factored_two_three_term16_t* term = rep_terms_[n];
  for (int i = 0; i < j; i++, term++) {
    a += term->a;
    int t = term->b & ~(1<<15);
    if (t > b) b = t;
  }
  return (j - 1) * cost.compose + a * cost.square + b * cost.cube;
}
