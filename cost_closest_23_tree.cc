#include "powcosts/cost_closest_23_tree.h"

extern "C" {
#include "liboptarith/closest_23.h"
}

double CostClosest23Tree::cost(const group_cost_t& cost,
			       const mpz_c& in_n) const {
  int term_count;
  two_three_term_t* terms =
      rep_prune_closest(&term_count, in_n.z, &cost, 16);
  int max_a = 0;
  int max_b = 0;
  for (int i = 0; i < term_count; i++) {
    if (terms[i].a > max_a) max_a = terms[i].a;
    if (terms[i].b > max_b) max_b = terms[i].b;
  }
  return cost.square * max_a
         + cost.cube * max_b
         + (term_count - 1) * cost.compose;
}

