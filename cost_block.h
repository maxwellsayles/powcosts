#pragma once
#ifndef COST_BLOCK_H_
#define COST_BLOCK_H_

#include <gmp.h>
#include <stdint.h>

#include "powcosts/i_cost_exp.h"

extern "C" {
#include "libqform/qform_group.h"
}

class CostBlock : public ICostExp {
 public:
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;
 private:
  double cost_u16(const group_cost_t& cost, const uint16_t n) const;
};

#endif

