#include "powcosts/cost_dbns_chain_r2l.h"

double cost_dbns_chain_r2l(const group_cost_t& costs,
			   const mpz_c& in_n) {
  mpz_c n(in_n);
  double res = 0;
  int i;
  while (mpz_cmp_ui(n.z, 0) > 0) {
    // remove powers of two
    i = mpz_scan1(n.z, 0);
    mpz_tdiv_q_2exp(n.z, n.z, i);
    res += costs.square * i;

    // remove powers of three
    while ((i = mpz_mod3(n.z)) == 0) {
      mpn_divexact_by3(n.z->_mp_d, n.z->_mp_d, n.z->_mp_size);
      res += costs.cube;
    }

    if (i == 1) {
      // subtract 1 to make it divisible by 3
      mpz_sub_ui(n.z, n.z, 1);
      res += costs.compose;
    } else {
      // i == 2
      // add 1 to make it divisible by 3
      mpz_add_ui(n.z, n.z, 1);
      res += costs.compose;
    }
  }
  return res;
}

double cost_dbns_chain_r2l36(const group_cost_t& costs,
			     const mpz_c& in_n) {
  mpz_c n(in_n);
  mpz_c t;
  double res = 0;
  int i;
  int m3;
  int m36;
  while (mpz_cmp_ui(n.z, 0) > 0) {
    // remove powers of two
    i = mpz_scan1(n.z, 0);
    mpz_tdiv_q_2exp(n.z, n.z, i);
    res += costs.square * i;

    // remove powers of three
    while ((m3 = mpz_mod3(n.z)) == 0) {
      mpn_divexact_by3(n.z->_mp_d, n.z->_mp_d, n.z->_mp_size);
      res += costs.cube;
    }

    // mod 36
    mpz_tdiv_q_2exp(t.z, n.z, 2);
    m36 = (mpz_mod9(t.z)<<2) | (n.z->_mp_d[0] & 3);

    if ((m3 == 1 && m36 != 7 && m36 != 31) ||
	(m36 == 5 || m36 == 17)) {
      // subtract 1
      mpz_sub_ui(n.z, n.z, 1);
      res += costs.compose;
    } else {
      // add 1
      mpz_add_ui(n.z, n.z, 1);
      res += costs.compose;
    }
  }
  return res;
}


