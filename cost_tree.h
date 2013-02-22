/**
 * Iterates on the k best candidates.
 * The derived class must implement children().
 */
#pragma once
#ifndef COST_TREE_H_
#define COST_TREE_H_

#include <set>

#include <gmp.h>

#include "powcosts/i_cost_exp.h"
#include "powcosts/partial_cost.h"

class CostTree : public ICostExp {
 public:
  explicit CostTree(const int k) : k_(k) {}
  double cost(const group_cost_t& cost, const mpz_c& in_n) const override;

 protected:
  virtual std::set<PartialCost> children(const group_cost_t& cost,
					 const PartialCost& n) const = 0;

 private:
  const int k_;
};

#endif

