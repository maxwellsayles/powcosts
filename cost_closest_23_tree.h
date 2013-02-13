#pragma once
#ifndef COST_CLOSEST_23_TREE_H_
#define COST_CLOSEST_23_TREE_H_

#include "powcosts/i_cost_exp.h"

class CostClosest23Tree : public ICostExp {
 public:
  double cost(const group_cost_t& cost, const mpz_c& in_n) const;
};

#endif

