#include "cost_naf.h"

double CostNafR2L::cost(const group_cost_t& costs, const mpz_c& in_n) const {
  mpz_c n(in_n);
  double res = 0;
  int c = 0;
  int m = 0;
  while (mpz_cmp_ui(n.z, 0) > 0) {
    m = n.z->_mp_d[0] & 3;  // n mod 4
    if (c == 0) {
      if (m == 1) {
	res += costs.compose;
      } else if (m == 3) {
	res += costs.compose;
	c = 1;
      }
    } else {
      if (m == 0) {
	res += costs.compose;
	c = 0;
      } else if (m == 2) {
	res += costs.compose;
      }
    }
    res += costs.square;
    mpz_tdiv_q_2exp(n.z, n.z, 1);
  }

  if (c == 1) {
    res += costs.compose;
  }
  return res;
}

