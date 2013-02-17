/**
 * @file cost_memo_chain.h
 * Recursively compute the miminum cost chain, but only allowing
 * a single term to be added or subtracted at each step.
 *
 * NOTE: This is only useful for small n (typically < 65536) since its
 * runtime is exponential in the size of the input (i.e. linear in the
 * value of the input).
 */
#pragma once
#ifndef COST_MEMO_CHAIN_H_
#define COST_MEMO_CHAIN_H_

#include <map>

#include <gmp.h>

#include "powcosts/i_cost_exp.h"

class CostMemoChain : public ICostExp {
 public:
  void clear_cache() { mem_.clear(); }
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;
 private:
  mutable std::map<mpz_c, double> mem_;
};

#endif

