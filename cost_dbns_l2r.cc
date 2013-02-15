#include "powcosts/cost_dbns_l2r.h"

#include <iomanip>
#include <iostream>
#include <limits>

#include <stdio.h>

extern "C" {
#include "liboptarith/primorial.h"
}

#include "powcosts/mpz_c.h"
#include "powcosts/util.h"

using namespace std;

// Compute the best DB approximation without changing max_a or max_b
// after each term.
static double dbns_l2r_bounded(const group_cost_t& costs,
			       const mpz_t in_n,
			       const int max_a,
			       const int max_b) {
  mpz_c n;
  mpz_c t;
  int cost_a = 0;
  int cost_b = 0;
  int a;
  int b;
  int terms = 0;
  
  n = in_n;
  while (mpz_cmp_ui(n.z, 0) != 0) {
    terms++;
    mpz_abs(n.z, n.z);
    mpz_c approx = best_db_approx(&a, &b, n.z, max_a, max_b);
    if (a > cost_a) cost_a = a;
    if (b > cost_b) cost_b = b;
    mpz_sub(n.z, n.z, approx.z);
  }

  return cost_a * costs.square +
         cost_b * costs.cube +
         (terms - 1) * costs.compose;
}

/**
 * Exponentiates using a DB-representation from left-to-right
 * Timing tests show that the high 10% of 'max_a' seems to be sufficient
 * given our costs on quadratic forms.
 */
static double dbns_l2r(const group_cost_t& costs, const mpz_t in_n) {
  mpz_c t;
  double best_cost = std::numeric_limits<double>::max();
  int k = mpz_sizeinbase(in_n, 3);
  int max_a = 0;
  int max_b = 0;

  while (max_b <= k) {
    mpz_ui_pow_ui(t.z, 3, max_b);
    mpz_tdiv_q(t.z, in_n, t.z);
    max_a = mpz_sizeinbase(t.z, 2);

    double cost = dbns_l2r_bounded(costs, in_n, max_a, max_b);
    if (cost < best_cost) {
      best_cost = cost;
    }

    max_b ++;
  }

  return best_cost;
}

/**
 * Exponentiates using a DB-representation from left-to-right
 */
static double dbns_chain_l2r_bounded(const group_cost_t& costs,
				     const mpz_t in_n,
				     const int in_max_a,
				     const int in_max_b) {
  mpz_c n(in_n);
  mpz_c t;
  double res = 0;
  int big_a = 0;
  int big_b = 0;
  int max_a = in_max_a;
  int max_b = in_max_b;
  int a;
  int b;
  int terms = 0;

  mpz_set(n.z, in_n);
  while (mpz_cmpabs_ui(n.z, 1) > 0) {
    terms ++;

    if (mpz_cmp_ui(n.z, 0) > 0) {
      mpz_c approx = best_db_approx(&a, &b, n.z, max_a, max_b);
      if (a > big_a) big_a = a;
      if (b > big_b) big_b = b;
      mpz_sub(n.z, n.z, approx.z);
    } else {
      mpz_abs(t.z, n.z);
      mpz_c approx = best_db_approx(&a, &b, t.z, max_a, max_b);
      if (a > big_a) big_a = a;
      if (b > big_b) big_b = b;
      mpz_add(n.z, n.z, approx.z);
    }
    max_a = a;
    max_b = b;
  }

  res += big_a * costs.square;
  res += big_b * costs.cube;
  if (terms > 1) {
    res += (terms-1) * costs.compose;
  }
  return res;
}

/**
 * Exponentiates using a DB-representation from left-to-right
 * Timing tests show that the high 10% of 'max_a' seems to be sufficient
 * given our costs on quadratic forms.
 */
double dbns_chain_l2r(const group_cost_t& costs, const mpz_t in_n) {
  mpz_c t;
  double best_cost = std::numeric_limits<double>::max();
  int k = mpz_sizeinbase(in_n, 2);
  int max_a = 9*k/10;
  int max_b;

  while (max_a <= k) {
    mpz_tdiv_q_2exp(t.z, in_n, max_a);
    max_b = mpz_sizeinbase(t.z, 3);

    double cost = dbns_chain_l2r_bounded(costs, in_n, max_a, max_b);
    if (cost < best_cost) best_cost = cost;

    max_a ++;
  }

  return best_cost;
}


double Cost_DBNS_L2R_Bounded::cost(const group_cost_t& cost,
				   const mpz_c& in_n) const {
  return dbns_l2r_bounded(cost, in_n.z, max_a_, max_b_);
}

double Cost_DBNS_L2R::cost(const group_cost_t& cost,
			   const mpz_c& in_n) const {
  return dbns_l2r(cost, in_n.z);
}

void graph_dbns_l2r_bounds(const group_cost_t& costs,
			   int primorial_index,
			   const std::string& filename) {
  const int sample_points = 200;
  remove(filename.c_str());

  mpz_c primorial;
  mpz_c tmp;

  mpz_primorial(primorial.z, primorial_index);
  const int bits = mpz_sizeinbase(primorial.z, 2);
  cout << "Primorial has " << bits << " bits." << endl;

  cout << setprecision(5) << fixed;
  for (int i = 0; i <= sample_points; i++) {
    int max_a = bits * i / sample_points;
    cout << "max_a=" << max_a << flush;

    mpz_tdiv_q_2exp(tmp.z, primorial.z, max_a);
    int max_b = mpz_sizeinbase(tmp.z, 3);

    double cost = dbns_l2r_bounded(costs, primorial.z, max_a, max_b);
    cout << " cost=" << cost << endl;
    // Write out cost in millis.
    append_gnuplot_datfile(filename, max_a, cost / 1000000);
  }

  cout << setprecision(5) << fixed;
  cout << "Time to exponentiate if left unbounded: "
       << dbns_l2r_bounded(costs, primorial.z,
			   numeric_limits<int>::max(),
			   numeric_limits<int>::max())
       << endl;
}
