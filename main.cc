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
#include "liboptarith/group_pow.h"
#include "liboptarith/math32.h"
#include "liboptarith/primes.h"
#include "liboptarith/primorial.h"
#include "libqform/costs_64.h"
#include "libqform/costs_128.h"
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

string dat_defence_file(const string& type,
			const int bits) {
  stringstream ss;
  ss << "dat-defence/" << type << '-' << bits << ".dat";
  return ss.str();
}

string dat_file(const string& type, const string& ext) {
  return "dat/" + type + "-" + ext + ".dat";
}

string dat16bit_file(const string& type, const string& ext) {
  return "dat-65536/" + type + "-" + ext + ".dat";
}

string datbound_file(const string& type, const string& ext) {
  return "dat-bound/" + type + "-" + ext + ".dat";
}

void time_real_primorial_growth(const string& out_file) {
  const int bits = 48;
  const int group_count = 1000;
  const int class_count = 100;
  assert(bits <= s64_qform_group_max_bits);
  cout << setprecision(5) << fixed;
  mpz_c primorial(1);
  remove(out_file.c_str());

  s64_qform_group_t qgroup;
  s64_qform_group_init(&qgroup);
  s64_qform_t A;
  s64_qform_t R;
  s64_qform_init(&qgroup, &A);
  s64_qform_init(&qgroup, &R);
  group_pow_t pow;
  group_pow_init(&pow, &qgroup.desc.group);
  mpz_t D;
  mpz_init(D);
  gmp_randstate_t rands;
  gmp_randinit_default(rands);

  for (int prime_index = 0;
       prime_index < prime_count;
       prime_index += prime_step) {
    // Compute the power primorial
    mpz_power_primorial(primorial.z, prime_index + 1,
			prime_list[prime_index] * prime_list[prime_index]);

    cout << "Using P_" << prime_index
         << " on a " << bits << "-bit discriminant." << endl;
    int primorial_size = mpz_sizeinbase(primorial.z, 2);
    cout << "Power primorial has " << primorial_size << " bits." << endl;

    // Generate 2,3 representation.
    cout << "Generating 2,3 represenation." << endl;
    int term_count;
    factored_two_three_term16_t* terms =
        factored_rep_prune_closest(&term_count, primorial.z,
				   &s64_qform_all_costs[bits-16], 16);


    // Estimate cost.
    int max_a = 0;
    int max_b = 0;
    for (int i = 0; i < term_count; i ++) {
      //      if (terms[i].a > max_a) max_a = terms[i].a;
      max_a += terms[i].a;
      int b = terms[i].b & 0x7f;
      if (b > max_b) max_b = b;
    }
    double est = s64_qform_all_costs[bits-16].compose * (term_count - 1) +
                 s64_qform_all_costs[bits-16].square * max_a +
                 s64_qform_all_costs[bits-16].cube * max_b;
    cout << "Estimated time is: " << (est / 1000) << endl;

    // Time exponentiation.
    cout << "Timing exponentiation." << endl;
    double cost = 0;
    uint64_t start = current_nanos();
    for (int g = 0; g < group_count; g++) {
      mpz_random_semiprime_discriminant(D, rands, bits);
      s64_qform_group_set_discriminant(&qgroup, D);
      for (int c = 0; c < class_count; c++) {
	qform_random_primeform(&qgroup.desc, &A);
	group_pow_factored23(&pow, &R, &A, terms, term_count);
      }
    }
    cost = current_nanos() - start;
    cost /= group_count * class_count;

    // Release the 2,3 representation.
    free(terms);

    cout << "Actual L2R Best Approximations:" << cost << endl;
    append_gnuplot_datfile(out_file, prime_index + 1, cost / 1000000);
    cout << endl;
  }

  mpz_clear(D);
  group_pow_clear(&pow);
  s64_qform_clear(&qgroup, &A);
  s64_qform_clear(&qgroup, &R);
  s64_qform_group_clear(&qgroup);
}


void time_primorial_growth(const group_cost_t& costs,
			   const string& type,
			   const int bits,
			   const string& out_file,
			   const ICostExp& cost_exp) {
  cout << setprecision(5) << fixed;
  mpz_c primorial(1);
  remove(out_file.c_str());

  for (int prime_index = 0;
       prime_index < prime_count;
       prime_index += prime_step) {
    // Compute the power primorial
    mpz_power_primorial(primorial.z, prime_index + 1,
			prime_list[prime_index] * prime_list[prime_index]);

    cout << "Using P_" << prime_index
         << " on a " << bits << "-bit discriminant." << endl;
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

void time_defence() {
  CostBinary         cost_binary;
  CostNafR2L         cost_naf_r2l;
  Cost_DBNS_L2R      cost_dbns_l2r(16);
  CostPM2a3b         cost_pm2a3b(4, 4, 4);
  CostClosest23Tree  cost_closest_23_tree(4);
  const fnc_desc descs[] = {
    {"binary", cost_binary},
    {"naf_r2l", cost_naf_r2l},
    {"dbns_l2r", cost_dbns_l2r},
    {"pm2a3b", cost_pm2a3b},
    {"dbns_l2r_tree", cost_closest_23_tree},
  };
  const int desc_count = sizeof(descs) / sizeof(fnc_desc);

  for (int bits = 16; bits <= s128_qform_group_max_bits; bits += 16) {
    for (int i = 0; i < desc_count; i++) {
      const fnc_desc& desc = descs[i];
      if (bits <= s64_qform_group_max_bits) {
	string outfile = dat_defence_file(desc.type, bits);
	time_primorial_growth(s64_qform_all_costs[bits-16],
			      desc.type, bits,
			      outfile, desc.cost_exp);
      } else {
	string outfile = dat_defence_file(desc.type, bits);
	time_primorial_growth(s128_qform_all_costs[bits-16],
			      desc.type, bits,
			      outfile, desc.cost_exp);
      }
    }
  }
}

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
    string outfile = dat_file(desc.type, "64");
    time_primorial_growth(s64_qform_costs,
			  desc.type, 64,
			  outfile, desc.cost_exp);
    outfile = dat_file(desc.type, "128");
    time_primorial_growth(s128_qform_costs,
			  desc.type, 128,
			  outfile, desc.cost_exp);
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

  //  time_defence();
  //  time_real_primorial_growth("real-times-48.dat");


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
  /*
  CostClosest23Tree::vary_max_leaves(
      s64_qform_costs, 100,
      datbound_file("dbns_l2r_tree_vary", "64"),
      200, 1);
  CostClosest23Tree::vary_max_leaves(
      s128_qform_costs, 100,
      datbound_file("dbns_l2r_tree_vary", "128"),
      200, 1);
  */
  return 0;
}


