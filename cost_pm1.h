/**
 * Iterates on the k best nodes,
 * where node x spawns (x+1)/2^c3^d and (x-1)/2^c3^d
 */
#pragma once
#ifndef COST_PM1_H_
#define COST_PM1_H_

#include <set>

#include <gmp.h>

#include "powcosts/cost_tree.h"
#include "powcosts/partial_cost.h"

class CostPM1 : public CostTree {
 public:
  explicit CostPM1(const int k) : CostTree(k) {}
  
 protected:
  std::set<PartialCost> children(const group_cost_t& cost,
				 const PartialCost& n) const override;
};

#endif

