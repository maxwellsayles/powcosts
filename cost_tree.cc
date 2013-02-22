#include "powcosts/cost_tree.h"

#include <set>
#include <vector>

#include <assert.h>

#include "powcosts/partial_cost.h"
#include "powcosts/util.h"

using namespace std;

typedef set<PartialCost> parts_t;

double CostTree::cost(const group_cost_t& cost,
		      const mpz_c& in_n) const {
  parts_t parts;

  // Reduce by 2 and 3.
  // TODO: reduce before calling children on the partial.
  mpz_c n(in_n);
  pair<int, int> res = n.reduce2_3();
  if (res.first != 0 || res.second != 0) {
    // Some reduction occurred, so we have our first term.
    parts.insert(PartialCost(cost, 1, res.first, res.second, n));
  } else {
    // No reduction occurred, so insert the input unchanged.
    parts.insert(PartialCost(cost, n));
  }

  while (true) {
    parts_t new_parts;
    assert(new_parts.size() == 0);
    for (auto part : parts) {
      // Generate the set of new candidates
      parts_t tmp_parts = children(cost, part);
      new_parts.insert(tmp_parts.begin(), tmp_parts.end());
    }

    // Take the k smallest partial costs.
    set<PartialCost> empty_set;
    parts.swap(empty_set);  // This releases the memory held by the set.
    auto part = new_parts.cbegin();
    for (int i = 0; i < k_ && part != new_parts.cend(); i++, ++part) {
      parts.insert(*part);
    }
    assert(parts.size() <= static_cast<size_t>(k_));

    // If the first element in the set has a remainder of 0,
    // then we are finished.
    auto iter = parts.begin();
    if (mpz_cmp_ui(iter->remainder->z, 0) == 0) {
      return iter->partial_cost();
    }
  }
  assert(false);
  return 0;
}
