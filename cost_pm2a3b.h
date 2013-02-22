/**
 * Iterates on the k best nodes,
 * where node x spawns (x+/-2^a3^b)/2^c3^d.
 */
#pragma once
#ifndef COST_PM2A3B_H_
#define COST_PM2A3B_H_

#include <set>

#include <gmp.h>

#include "powcosts/cost_tree.h"
#include "powcosts/partial_cost.h"

class CostPM2a3b : public CostTree {
 public:
  explicit CostPM2a3b(const int k) : CostTree(k) {}
  
 protected:
  std::set<PartialCost> children(const group_cost_t& cost,
				 const PartialCost& n) const override;
};

#endif

