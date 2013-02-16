/**
 * @file cost_add_chain.h
 * Computes optimal additive-only strictly chains partitions.
 * Expressions are of the form
 * n = 2^a1*3^b1 + 2^a2*3^b3 + ... 2^an*3^bn
 * where 2^ai*3^bi is strictly less than and divides 2^aj*3^bj for all i < j.
 *
 * NOTE: This is only useful for small n (typically < 65536) since its
 * runtime is exponential in the size of the input (i.e. linear in the
 * value of the input).
 */
#pragma once
#ifndef COST_ADD_CHAIN_H_
#define COST_ADD_CHAIN_H_

#include <map>

#include <gmp.h>

#include "powcosts/i_cost_exp.h"

class CostAddChain : public ICostExp {
 public:
  void clear_cache() { mem_.clear(); }
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;
 private:
  mutable std::map<mpz_c, double> mem_;
};

#endif

