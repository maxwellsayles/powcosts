#include "powcosts/cost_pm2a3b.h"

#include <set>

#include <assert.h>

extern "C" {
#include "liboptarith/primorial.h"
}

#include "powcosts/partial_cost.h"
#include "powcosts/util.h"

using namespace std;

set<PartialCost> CostPM2a3b::children(const group_cost_t& cost,
				       const PartialCost& part) const {
  set<PartialCost> res;
  mpz_c v2;
  mpz_c v3;

  mpz_set_ui(v2.z, 1);
  for (int a = 0; a <= A_; a++) {
    mpz_set(v3.z, v2.z);
    for (int b = 0; b <= B_; b++) {
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

void CostPM2a3b::vary_max_bounds(const group_cost_t& costs,
				 const int primorial_index,
				 const std::string& filename,
				 const int k,
				 const int sample_points) {
  remove(filename.c_str());

  mpz_c primorial;
  mpz_c tmp;

  mpz_primorial(primorial.z, primorial_index);
  const int bits = mpz_sizeinbase(primorial.z, 2);
  cout << "Primorial has " << bits << " bits." << endl;

  cout << setprecision(5) << fixed;
  for (int i = 0; i <= sample_points; i++) {
    cout << "bound=" << i << flush;
    CostPM2a3b cost_exp(k, i, i);
    double cost = cost_exp.cost(costs, primorial.z);
    cout << " cost=" << cost << endl;
    // Write out cost in millis.
    append_gnuplot_datfile(filename, i, cost / 1000000);
  }
}
