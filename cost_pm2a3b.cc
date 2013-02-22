#include "powcosts/cost_pm2a3b.h"

#include <set>

#include <assert.h>

#include "powcosts/partial_cost.h"
#include "powcosts/util.h"

using namespace std;

set<PartialCost> CostPM2a3b::children(const group_cost_t& cost,
				       const PartialCost& part) const {
  set<PartialCost> res;
  mpz_c v2;
  mpz_c v3;

  const int A = 16;
  const int B = 16;

  mpz_set_ui(v2.z, 1);
  for (int a = 0; a <= A; a++) {
    mpz_set(v3.z, v2.z);
    for (int b = 0; b <= B; b++) {
      // Add 2^a * 3^b
      auto A = make_shared<mpz_c>(*part.remainder.get());
      mpz_add(A->z, A->z, v3.z);
      res.insert(PartialCost(cost,
			     part.terms + 1,
			     max(part.squares, a),
			     max(part.cubes, b),
			     A));
      
      // Subtract 2^a * 3^b
      auto B = make_shared<mpz_c>(*part.remainder.get());
      mpz_sub(B->z, B->z, v3.z);
      mpz_abs(B->z, B->z);
      res.insert(PartialCost(cost,
			     part.terms + 1,
			     max(part.squares, a),
			     max(part.cubes, b),
			     B));

      mpz_mul_ui(v3.z, v3.z, 3);
    }
    mpz_mul_2exp(v2.z, v2.z, 1);
  }
  
  return res;
}

