#include "powcosts/cost_binary.h"

double CostBinary::cost(const group_cost_t& cost, const mpz_c& in_n) const {
  int terms = mpz_sizeinbase(in_n.z, 2);
  int ones = mpz_popcount(in_n.z);
  return cost.square * (terms - 1) + cost.compose * ones;
}
