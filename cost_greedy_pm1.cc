#include "powcosts/cost_greedy_pm1.h"

#include <functional>
#include <queue>
#include <set>
#include <vector>

#include <assert.h>

#include "powcosts/partial_cost.h"
#include "powcosts/util.h"

using namespace std;

typedef set<PartialCost> parts_t;
//typedef priority_queue<PartialCost,
//		       vector<PartialCost>,
//		       greater<PartialCost>> parts_t;

double CostGreedyPM1::cost(const group_cost_t& cost,
			   const mpz_c& in_n) const {
  parts_t parts;

  // Reduce by 2 and 3.
  mpz_c n(in_n);
  pair<int, int> res = n.reduce2_3();
  if (res.first != 0 || res.second != 0) {
    // Some reduction occurred, so we have our first term.
    parts.insert(PartialCost(cost, 1, res.first, res.second, n));
    //    parts.emplace(cost, 1, res.first, res.second, n);
  } else {
    // No reduction occurred, so insert the input unchanged.
    parts.insert(PartialCost(cost, n));
    //    parts.emplace(cost, n);
  }

  while (true) {
    parts_t new_parts;
    assert(new_parts.size() == 0);
    for (auto part : parts) {
      //    while (!parts.empty()) {
      //      const PartialCost& part = parts.top();
      // Add 1
      auto A = make_shared<mpz_c>(*part.remainder.get());
      mpz_add_ui(A->z, A->z, 1);
      auto A_reduce = A->reduce2_3();
      //      new_parts.emplace(cost,
      //			part.terms + 1,
      //			part.squares + A_reduce.first,
      //			part.cubes + A_reduce.second,
      //			A);
      new_parts.insert(PartialCost(cost,
      				   part.terms + 1,
      				   part.squares + A_reduce.first,
      				   part.cubes + A_reduce.second,
      				   A));

      // Subtract 1
      auto B = make_shared<mpz_c>(*part.remainder.get());
      mpz_sub_ui(B->z, B->z, 1);
      auto B_reduce = B->reduce2_3();
      //      new_parts.emplace(cost,
      //			part.terms + 1,
      //			part.squares + B_reduce.first,
      //			part.cubes + B_reduce.second,
      //			B);
      new_parts.insert(PartialCost(cost,
      				   part.terms + 1,
				   part.squares + B_reduce.first,
				   part.cubes + B_reduce.second,
				   B));
      //      parts.pop();
    }

    // Take the k smallest partial costs.
    set<PartialCost> empty_set;
    parts.swap(empty_set);  // This releases the memory held by the set.
    auto part = new_parts.cbegin();
    for (int i = 0; i < k_ && part != new_parts.cend(); i++, ++part) {
      parts.insert(*part);
    }
    // TODO: This could be made faster (I think).
    //    for (int i = 0; i < k_ && !new_parts.empty(); i++) {
    //      parts.push(new_parts.top());
    //      new_parts.pop();
    //    }
    assert(parts.size() <= static_cast<size_t>(k_));

    // If the first element in the set has a remainder of 0,
    // then we are finished.
    //    if (mpz_cmp_ui(parts.top().remainder->z, 0) == 0) {
    //      return parts.top().partial_cost();
    //    }
    auto iter = parts.begin();
    if (mpz_cmp_ui(iter->remainder->z, 0) == 0) {
      return iter->partial_cost();
    }
  }
  assert(false);
  return 0;
}
