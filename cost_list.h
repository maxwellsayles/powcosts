/**
 * Exponentiate using precomputed 16-bit representations.
 * NOTE: This class assumes that the input exponent is the product of
 * primes <= 65535.  If the input exponent is not, the function will exit().
 */
#pragma once
#ifndef COST_LIST_H_
#define COST_LIST_H_

#include <gmp.h>
#include <stdint.h>

#include "powcosts/i_cost_exp.h"

extern "C" {
#include "libqform/qform_group.h"
}

class CostList : public ICostExp {
 public:
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;
 private:
  double cost_u16(const group_cost_t& cost, const uint16_t n) const;
};

#endif

