/**
 * @file i_cost_exp.h
 * Interface for classes that compute the cost of exponentiation.
 */
#pragma once
#ifndef I_COST_EXP_H_
#define I_COST_EXP_H_

#include <gmp.h>

#include "liboptarith/group.h"
#include "powcosts/mpz_c.h"

class ICostExp {
 public:
  virtual ~ICostExp() {}
  virtual double cost(const group_cost_t& costs, const mpz_c& n) const = 0;
};

#endif

