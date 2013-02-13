#include "powcosts/cost_dbns_chain_r2l.h"

double CostDBNSChainR2L::cost(const group_cost_t& costs,
			      const mpz_c& in_n) const {
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

double CostDBNSChainR2L36::cost(const group_cost_t& costs,
				const mpz_c& in_n) const {
  mpz_c n(in_n);
  mpz_c t;
  double res = 0;
  int i;
  int m3;
  int m36;
  while (mpz_cmp_ui(n.z, 3) > 0) {
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

    if (m36 ==  1 || m36 ==  5 || m36 == 13 ||
	m36 == 17 || m36 == 25 || m36 == 29) {
      mpz_sub_ui(n.z, n.z, 1);
    } else {
      mpz_add_ui(n.z, n.z, 1);
    }
    res += costs.compose;
  }
  if (mpz_cmp_ui(n.z, 2) == 0) {
    res += costs.square;
  }
  if (mpz_cmp_ui(n.z, 3) == 0) {
    res += costs.cube;
  }
  res += costs.compose;
  return res;
}

double CostDBNSChainR2L36Prog::cost(const group_cost_t& costs,
				    const mpz_c& in_n) const {
  const int mod_to_bit[] =
  //   0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35
      {0, 1, 0, 0, 0, 2, 0, 3, 0, 0,  0,  4,  0,  5,  0,  0,  0,  6,  0,  7,  0,  0,  0,  8,  0,  9,  0,  0,  0, 10,  0, 11,  0,  0,  0, 12};
  mpz_c n(in_n);
  mpz_c t;
  double res = 0;
  int i;
  int m3;
  int m36;
  while (mpz_cmp_ui(n.z, 3) > 0) {
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

    // 1, 5, 7, 11, 13, 17, 19, 23, 25, 29, 31, 35
    if (mask_ & (1 << (mod_to_bit[m36]-1))) {
      mpz_sub_ui(n.z, n.z, 1);
    } else {
      mpz_add_ui(n.z, n.z, 1);
    }
    res += costs.compose;
  }
  if (mpz_cmp_ui(n.z, 3) == 0) {
    res += costs.cube;
  }
  if (mpz_cmp_ui(n.z, 2) == 0) {
    res += costs.square;
  }
  res += costs.compose;
  return res;
}


