/**
 * @file main.cc
 * Compute the costs of various exponentiation methods using a variety
 * of primorials.
 */
#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <vector>

#include <gmp.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "powcosts/cost_add_chain.h"
#include "powcosts/cost_binary.h"
#include "powcosts/cost_block.h"
#include "powcosts/cost_closest_23_tree.h"
#include "powcosts/cost_dbns_chain_r2l.h"
#include "powcosts/cost_dbns_l2r.h"
#include "powcosts/cost_list.h"
#include "powcosts/cost_memo_chain.h"
#include "powcosts/cost_naf.h"
#include "powcosts/cost_pm1.h"
#include "powcosts/cost_pm2a3b.h"
#include "powcosts/cost_pm2apm3b.h"
#include "powcosts/cost_pre.h"
#include "powcosts/i_cost_exp.h"
#include "powcosts/mpz_c.h"
#include "powcosts/util.h"

extern "C" {
#include "liboptarith/closest_23.h"
#include "liboptarith/group.h"
#include "liboptarith/math32.h"
#include "liboptarith/primes.h"
#include "liboptarith/primorial.h"
#include "libqform/dbreps/mpz_pow_reps.h"
#include "libqform/dbreps/s128_pow_reps.h"
#include "libqform/dbreps/s64_pow_reps.h"
#include "libqform/mpz_qform.h"
#include "libqform/s64_qform.h"
#include "libqform/s128_qform.h"
}

using namespace std;

const int prime_count = 1000;
const int prime_step  = prime_count/100;

string dat_file(const string& type, const string& ext) {
  return "dat/" + type + "-" + ext + ".dat";
}

string dat16bit_file(const string& type, const string& ext) {
  return "dat-65536/" + type + "-" + ext + ".dat";
}

string datbound_file(const string& type, const string& ext) {
  return "dat-bound/" + type + "-" + ext + ".dat";
}

void time_primorial_growth(const group_cost_t& costs,
			   const string& type,
			   const string& ext,
			   const ICostExp& cost_exp) {
  cout << setprecision(5) << fixed;
  mpz_c primorial(1);
  const string out_file = dat_file(type, ext);
  remove(out_file.c_str());

  for (int prime_index = 0;
       prime_index < prime_count;
       prime_index += prime_step) {
    // Compute the power primorial
    mpz_power_primorial(primorial.z, prime_index + 1,
			prime_list[prime_index] * prime_list[prime_index]);

    cout << "Using P_" << prime_index
         << " on a " << ext << "-bit discriminant." << endl;
    int primorial_size = mpz_sizeinbase(primorial.z, 2);
    cout << "Power primorial has " << primorial_size << " bits." << endl;

    // Compute time of function (in millis).
    double c = cost_exp.cost(costs, primorial);
    cout << type << ": " << c << endl;
    append_gnuplot_datfile(out_file, prime_index + 1, c / 1000000);
    cout << endl;
  }
}

/// Time exponentiation in the range [min_value, max_value] inclusive.
void time_range(const group_cost_t& costs,
		const string& type,
		const string& ext,
		const ICostExp& cost_exp,
		const int min_value,
		const int max_value,
		const int step_value) {
  cout << setprecision(5) << fixed;
  const string out_file = dat16bit_file(type, ext);
  remove(out_file.c_str());

  for (int i = min_value; i <= max_value; i += step_value) {
    // Compute time of function.
    cout << "Exponent " << i << " on "
	 << ext << "-bit implementation." << endl;
    double c = cost_exp.cost(costs, i);
    cout << type << ": " << c << endl;
    append_gnuplot_datfile(out_file, i, c);

    cout << endl;
  }
}

struct fnc_desc {
  const string& type;
  const ICostExp& cost_exp;
};

void time_methods() {
  CostBinary         cost_binary;
  CostBlock          cost_block;
  CostDBNSChainR2L   cost_dbns_r2l;
  CostDBNSChainR2L36 cost_dbns_r2l36;
  Cost_DBNS_L2R      cost_dbns_l2r(16);
  CostList           cost_list;
  CostNafR2L         cost_naf_r2l;
  CostPM1            cost_pm1(16);
  CostPM2a3b         cost_pm2a3b(4, 4, 4);
  CostPM2aPM3b       cost_pm2apm3b(4, 4, 4);
  CostClosest23Tree  cost_closest_23_tree(16);
  const fnc_desc descs[] = {
    {"binary", cost_binary},
    {"block", cost_block},
    {"dbns_r2l", cost_dbns_r2l},
    {"dbns_r2l36", cost_dbns_r2l36},
    {"dbns_l2r", cost_dbns_l2r},
    {"list", cost_list},
    {"naf_r2l", cost_naf_r2l},
    {"pm1", cost_pm1},
    {"pm2a3b", cost_pm2a3b},
    {"pm2apm3b", cost_pm2apm3b},
    {"dbns_l2r_tree", cost_closest_23_tree},
  };
  const int desc_count = sizeof(descs) / sizeof(fnc_desc);

  for (int i = 0; i < desc_count; i++) {
    const fnc_desc& desc = descs[i];
    time_primorial_growth(s64_qform_costs,
			  desc.type, "64", desc.cost_exp);
    time_primorial_growth(s128_qform_costs,
			  desc.type, "128", desc.cost_exp);
  }
}

void time_16bit_methods() {
  /*
  // Memoized recursive chains
  time_range(s64_qform_costs,
	     "add_chain_65536", "64", CostAddChain(),
	     1, 65535, 1);
  time_range(s128_qform_costs,
  	     "add_chain_65536", "128", CostAddChain(),
	     1, 65535, 1);
  time_range(s64_qform_costs,
	     "memo_chain_65536", "64", CostMemoChain(),
	     1, 65535, 1);
  time_range(s128_qform_costs,
  	     "memo_chain_65536", "128", CostMemoChain(),
	     1, 65535, 1);

  // Incrementally searched representatioins
  time_range(s64_qform_costs,
	     "pre_65536", "64", CostPre(s64_pow_reps, s64_pow_rep_sizes),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "pre_65536", "128", CostPre(s128_pow_reps, s128_pow_rep_sizes),
	     1, 65535, 1);

  // Binary and NAF
  time_range(s64_qform_costs,
	     "binary_65536", "64", CostBinary(),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "binary_65536", "128", CostBinary(),
	     1, 65535, 1);
  time_range(s64_qform_costs,
	     "naf_r2l_65536", "64", CostNafR2L(),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "naf_r2l_65536", "128", CostNafR2L(),
	     1, 65535, 1);

  // dbns r2l
  time_range(s64_qform_costs,
	     "dbns_r2l_65536", "64", CostDBNSChainR2L(),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "dbns_r2l_65536", "128", CostDBNSChainR2L(),
	     1, 65535, 1);
  time_range(s64_qform_costs,
	     "dbns_r2l36_65536", "64", CostDBNSChainR2L36(),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "dbns_r2l36_65536", "128", CostDBNSChainR2L36(),
	     1, 65535, 1);

  // dbns l2r
  time_range(s64_qform_costs,
	     "dbns_l2r_65536", "64", Cost_DBNS_L2R(),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "dbns_l2r_65536", "128", Cost_DBNS_L2R(),
	     1, 65535, 1);

  // dbns l2r tree
  time_range(s64_qform_costs,
	     "dbns_l2r_tree_65536", "64", CostClosest23Tree(16),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "dbns_l2r_tree_65536", "128", CostClosest23Tree(16),
	     1, 65535, 1);
  */

  // greedy pm1
  time_range(s64_qform_costs,
	     "pm1_65536", "64", CostPM1(16),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "pm1_65536", "128", CostPM1(16),
	     1, 65535, 1);

  // greedy +/- 2^a*3^b
  time_range(s64_qform_costs,
	     "pm2a3b_65536", "64", CostPM2a3b(4, 4, 4),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "pm2a3b_65536", "128", CostPM2a3b(4, 4, 4),
	     1, 65535, 1);

  // greedy +/- 2^a +/- 3^b
  time_range(s64_qform_costs,
	     "pm2apm3b_65536", "64", CostPM2aPM3b(4, 4, 4),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "pm2apm3b_65536", "128", CostPM2aPM3b(4, 4, 4),
	     1, 65535, 1);
}


int main(int argc, char** argv) {
  struct rlimit l = {1024ULL*1024ULL*1024ULL, 1024ULL*1024ULL*1024ULL};
  setrlimit(RLIMIT_AS, &l);

  time_methods();
  //  time_16bit_methods();

  /*
  Cost_DBNS_L2R::vary_max_bounds(s64_qform_costs, 1000,
				 datbound_file("dbns_l2r_vary_max", "64"));
  Cost_DBNS_L2R::vary_max_bounds(s128_qform_costs, 1000,
				 datbound_file("dbns_l2r_vary_max", "128"));
  */
  /*
  CostPM2aPM3b::vary_max_bounds(s64_qform_costs, 1000,
			      datbound_file("pm2apm3b_vary_max", "64"), 4, 32);
  CostPM2aPM3b::vary_max_bounds(s128_qform_costs, 1000,
			     datbound_file("pm2apm3b_vary_max", "128"), 4, 32);
  */
  return 0;
}


