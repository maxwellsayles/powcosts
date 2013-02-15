#pragma once
#ifndef COST_BINARY_H_
#define COST_BINARY_H_

#include <gmp.h>

#include "powcosts/i_cost_exp.h"

class CostBinary : public ICostExp {
 public:
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;
};

#endif

