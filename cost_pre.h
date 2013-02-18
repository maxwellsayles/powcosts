#pragma once
#ifndef COST_PRE_H_
#define COST_PRE_H_

#include <gmp.h>

#include "powcosts/i_cost_exp.h"

extern "C" {
#include "libqform/qform_group.h"
}

class CostPre : public ICostExp {
 public:
  CostPre(const factored_two_three_term16_t* const* rep_terms,
	  const int* rep_sizes)
    : rep_terms_(rep_terms)
    , rep_sizes_(rep_sizes)
  {
  }
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;
 private:
  const factored_two_three_term16_t* const* rep_terms_;
  const int* rep_sizes_;
};

#endif

