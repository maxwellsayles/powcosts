#pragma once
#ifndef COST_NAF_H_
#define COST_NAF_H_

#include "powcosts/i_cost_exp.h"

class CostNafR2L : public ICostExp {
 public:
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;
};

#endif

