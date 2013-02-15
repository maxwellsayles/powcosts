#pragma once
#ifndef COST_DBNS_L2R_H_
#define COST_DBNS_L2R_H_

#include <string>

#include <gmp.h>

#include "powcosts/i_cost_exp.h"

class Cost_DBNS_L2R_Bounded : public ICostExp {
 public:
  Cost_DBNS_L2R_Bounded(const int max_a, const int max_b)
    : max_a_(max_a)
    , max_b_(max_b)
  {
  }
  double cost(const group_cost_t& cost, const mpz_c& in_n) const;
 private:
  const int max_a_;
  const int max_b_;
};

void graph_dbns_l2r_bounds(const group_cost_t& cost,
			   int primorial,
			   const std::string& filename);

#endif
