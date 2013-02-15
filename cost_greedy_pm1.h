#pragma once
#ifndef COST_GREEDY_PM1_H_
#define COST_GREEDY_PM1_H_

#include <gmp.h>

#include "powcosts/i_cost_exp.h"

class CostGreedyPM1 : public ICostExp {
 public:
  explicit CostGreedyPM1(const int k) : k_(k) {}
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;
 private:
  int k_;
};

#endif

