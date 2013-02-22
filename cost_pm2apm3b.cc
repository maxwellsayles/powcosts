#include "powcosts/cost_pm2apm3b.h"

#include <set>

#include <assert.h>

#include "powcosts/partial_cost.h"
#include "powcosts/util.h"

using namespace std;

set<PartialCost> CostPM2aPM3b::children(const group_cost_t& cost,
					const PartialCost& part) const {
  set<PartialCost> res;
  shared_ptr<mpz_c> v;
  mpz_c v2;
  mpz_c v3;

  mpz_set_ui(v2.z, 1);
  for (int a = 0; a <= A_; a++) {
    mpz_set_ui(v3.z, 1);
    for (int b = 0; b <= B_; b++) {
      // + 2^a + 3^b
      v = make_shared<mpz_c>(*part.remainder.get());
      mpz_add(v->z, v->z, v2.z);
      mpz_add(v->z, v->z, v3.z);
      res.insert(PartialCost(cost,
			     part.terms + 2,
			     max(part.squares, a),
			     max(part.cubes, b),
			     v));
      
      // + 2^a - 3^b
      v = make_shared<mpz_c>(*part.remainder.get());
      mpz_add(v->z, v->z, v2.z);
      mpz_sub(v->z, v->z, v3.z);
      mpz_abs(v->z, v->z);
      res.insert(PartialCost(cost,
			     part.terms + 2,
			     max(part.squares, a),
			     max(part.cubes, b),
			     v));
      
      // - 2^a + 3^b
      v = make_shared<mpz_c>(*part.remainder.get());
      mpz_sub(v->z, v->z, v2.z);
      mpz_add(v->z, v->z, v3.z);
      mpz_abs(v->z, v->z);
      res.insert(PartialCost(cost,
			     part.terms + 2,
			     max(part.squares, a),
			     max(part.cubes, b),
			     v));
      
      // - 2^a - 3^b
      v = make_shared<mpz_c>(*part.remainder.get());
      mpz_sub(v->z, v->z, v2.z);
      mpz_sub(v->z, v->z, v3.z);
      mpz_abs(v->z, v->z);
      res.insert(PartialCost(cost,
			     part.terms + 2,
			     max(part.squares, a),
			     max(part.cubes, b),
			     v));
      

      mpz_mul_ui(v3.z, v3.z, 3);
    }
    mpz_mul_2exp(v2.z, v2.z, 1);
  }
  
  return res;
}

