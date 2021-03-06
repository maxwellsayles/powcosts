/**
 * Iterates on the k best nodes,
 * where node x spawns (x +/- 2^a3^b)/2^c3^d.
 */
#pragma once
#ifndef COST_PM2A3B_H_
#define COST_PM2A3B_H_

#include <set>
#include <string>

#include <gmp.h>

#include "powcosts/cost_tree.h"
#include "powcosts/partial_cost.h"

class CostPM2a3b : public CostTree {
 public:
  explicit CostPM2a3b(const int k, const int A, const int B)
    : CostTree(k)
    , A_(A)
    , B_(B)
  {
  }

  static void vary_max_bounds(const group_cost_t& costs,
			      const int primorial_index,
			      const std::string& filename,
			      const int k,
			      const int sample_points);
  
 protected:
  std::set<PartialCost> children(const group_cost_t& cost,
				 const PartialCost& n) const override;

 private:
  int A_;  // Bound on squares.
  int B_;  // Bound on cubes.
};

#endif

