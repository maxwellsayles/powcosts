#include "powcosts/cost_tree.h"

#include <set>

#include <assert.h>

#include "powcosts/partial_cost.h"
#include "powcosts/util.h"

using namespace std;

typedef set<PartialCost> parts_t;

double CostTree::cost(const group_cost_t& cost,
		      const mpz_c& in_n) const {
  parts_t parts;
  parts.insert(PartialCost(cost, 1, 0, 0, make_shared<mpz_c>(in_n)));

  while (true) {
    // Reduce the set of partial costs.
    parts_t reduced;
    for (auto part : parts) {
      auto n = make_shared<mpz_c>(*part.remainder.get());
      auto res = n->reduce2_3();
      reduced.insert(PartialCost(cost,
				 part.terms,
				 part.squares + res.first,
				 part.cubes + res.second,
				 n));
    }

    // If the first element in the set has a remainder of 0 or 1,
    // then we are finished.
    auto iter = reduced.cbegin();
    if (mpz_cmp_ui(iter->remainder->z, 0) == 0 ||
	mpz_cmp_ui(iter->remainder->z, 1) == 0) {
      return iter->partial_cost();
    }

    // Generate all the children.
    parts_t new_parts;
    for (auto part : reduced) {
      parts_t kids = children(cost, part);
      new_parts.insert(kids.begin(), kids.end());
    }

    // Take the k smallest partial costs.
    parts.clear();
    auto hint = parts.begin();
    auto part = new_parts.cbegin();
    for (int i = 0; i < k_ && part != new_parts.cend(); i++, ++part) {
      hint = parts.insert(hint, *part);
    }
    assert(parts.size() <= static_cast<size_t>(k_));
  }

  assert(false);
  return 0;
}
