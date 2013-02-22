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

    // Take the k smallest partial costs.
    parts_t k_parts;
    auto hint = k_parts.begin();
    auto part = reduced.cbegin();
    for (int i = 0; i < k_ && part != reduced.cend(); i++, ++part) {
      hint = k_parts.insert(hint, *part);
    }
    assert(k_parts.size() <= static_cast<size_t>(k_));
    
    // If the first element in the set has a remainder of 0 or 1,
    // then we are finished.
    auto iter = k_parts.cbegin();
    if (mpz_cmp_ui(iter->remainder->z, 0) == 0 ||
	mpz_cmp_ui(iter->remainder->z, 1) == 0) {
      return iter->partial_cost();
    }

    // Generate all the children.
    parts.clear();
    for (auto part : k_parts) {
      parts_t kids = children(cost, part);
      parts.insert(kids.begin(), kids.end());
    }
  }

  assert(false);
  return 0;
}
