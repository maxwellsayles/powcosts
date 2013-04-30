#include "powcosts/cost_closest_23_tree.h"

#include <iomanip>
#include <iostream>
#include <string>

#include <stdio.h>

extern "C" {
#include "liboptarith/closest_23.h"
#include "liboptarith/primorial.h"
}

#include "powcosts/mpz_c.h"
#include "powcosts/util.h"

using namespace std;

double CostClosest23Tree::cost(const group_cost_t& cost,
			       const mpz_c& in_n) const {
  int term_count;
  two_three_term_t* terms =
      rep_prune_closest(&term_count, in_n.z, &cost, k_);
  int max_a = 0;
  int max_b = 0;
  for (int i = 0; i < term_count; i++) {
    if (terms[i].a > max_a) max_a = terms[i].a;
    if (terms[i].b > max_b) max_b = terms[i].b;
  }
  return cost.square * max_a
         + cost.cube * max_b
         + (term_count - 1) * cost.compose;
}

void CostClosest23Tree::vary_max_leaves(const group_cost_t& costs,
					const int primorial_index,
					const std::string& filename,
					const int leaves_max,
					const int leaves_step) {
  remove(filename.c_str());

  mpz_c primorial;
  mpz_c tmp;

  mpz_primorial(primorial.z, primorial_index);
  const int bits = mpz_sizeinbase(primorial.z, 2);
  cout << "Primorial has " << bits << " bits." << endl;

  cout << setprecision(5) << fixed;
  for (int i = 1; i <= leaves_max; i += leaves_step) {
    cout << "bound=" << i << flush;
    CostClosest23Tree cost_exp(i);
    double cost = cost_exp.cost(costs, primorial.z);
    cout << " cost=" << cost << endl;
    // Write out cost in millis.
    append_gnuplot_datfile(filename, i, cost / 1000000);
  }
}
