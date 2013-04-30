#pragma once
#ifndef COST_CLOSEST_23_TREE_H_
#define COST_CLOSEST_23_TREE_H_

#include <string>

#include "powcosts/i_cost_exp.h"

class CostClosest23Tree : public ICostExp {
 public:
  explicit CostClosest23Tree(const int k) : k_(k) {}
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;

  static void vary_max_leaves(const group_cost_t& costs,
			      const int primorial_index,
			      const std::string& filename,
			      const int leaves_max,
			      const int leaves_step);
 private:
  const int k_;
};

#endif

