#include "powcosts/cost_greedy_pm1.h"

#include <functional>
#include <queue>
#include <set>
#include <vector>

#include <assert.h>

#include "powcosts/partial_cost.h"
#include "powcosts/util.h"

using namespace std;

set<PartialCost> CostGreedyPM1::children(const group_cost_t& cost,
					 const PartialCost& part) const {
  set<PartialCost> res;

  // Add 1
  auto A = make_shared<mpz_c>(*part.remainder.get());
  mpz_add_ui(A->z, A->z, 1);
  res.insert(PartialCost(cost,
			 part.terms + 1,
			 part.squares,
			 part.cubes,
			 A));

  // Subtract 1
  auto B = make_shared<mpz_c>(*part.remainder.get());
  mpz_sub_ui(B->z, B->z, 1);
  res.insert(PartialCost(cost,
			 part.terms + 1,
			 part.squares,
			 part.cubes,
			 B));

  return res;
}

