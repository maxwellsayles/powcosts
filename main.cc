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

#include "term.h"
#include "mpz_c.h"
#include "prune_c.h"
#include "rep_c.h"

#include "powcosts/cost_add_chain.h"
#include "powcosts/cost_binary.h"
#include "powcosts/cost_block.h"
#include "powcosts/cost_closest_23_tree.h"
#include "powcosts/cost_dbns_chain_r2l.h"
#include "powcosts/cost_dbns_l2r.h"
#include "powcosts/cost_greedy_pm1.h"
#include "powcosts/cost_memo_chain.h"
#include "powcosts/cost_naf.h"
#include "powcosts/cost_pre.h"
#include "powcosts/i_cost_exp.h"
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

const int prime_count = 2500;
const int prime_step  = prime_count/100;

/**
 * Computes min|N-3^b| and min|N-2*3^b|, then subtracts the smallest and repeats.
 */
double cost_greedy3(const group_cost_t& costs, const mpz_t in_n) {
  static mpz_c n;
  static mpz_c v;
  static mpz_c v2;
  static mpz_c d;
  static mpz_c best;

  int cost3 = 0;
  int cost2 = 0;
  int term_count = 0;
  int best3 = 0;
  int best2 = 0;

  mpz_set(n.z, in_n);
  while (mpz_cmpabs_ui(n.z, 0) != 0) {
    mpz_set(best.z, n.z);

    int b = mpz_sizeinbase(n.z, 3)-1;

    // try 3^b <= N
    mpz_ui_pow_ui(v.z, 3, b);
    if (n.z->_mp_size < 0) {
      mpz_add(d.z, n.z, v.z);
    } else {
      mpz_sub(d.z, n.z, v.z);
    }
    if (mpz_cmpabs(d.z, best.z) < 0) {
      mpz_set(best.z, d.z);
      best2 = 0;
      best3 = b;
    }
        
    // try 2*3^b
    mpz_mul_2exp(v2.z, v.z, 1);
    if (n.z->_mp_size < 0) {
      mpz_add(d.z, n.z, v2.z);
    } else {
      mpz_sub(d.z, n.z, v2.z);
    }
    if (mpz_cmpabs(d.z, best.z) < 0) {
      mpz_set(best.z, d.z);
      best2 = 1;
      best3 = b;
    }

    // try 3^{b+1}
    mpz_add(v.z, v.z, v2.z);
    if (n.z->_mp_size < 0) {
      mpz_add(d.z, n.z, v2.z);
    } else {
      mpz_sub(d.z, n.z, v2.z);
    }
    if (mpz_cmpabs(d.z, best.z) < 0) {
      mpz_set(best.z, d.z);
      best2 = 0;
      best3 = b+1;
    }

    // try 2*3^{b+1}
    mpz_mul_2exp(v2.z, v.z, 1);
    if (n.z->_mp_size < 0) {
      mpz_add(d.z, n.z, v2.z);
    } else {
      mpz_sub(d.z, n.z, v2.z);
    }
    if (mpz_cmpabs(d.z, best.z) < 0) {
      mpz_set(best.z, d.z);
      best2 = 1;
      best3 = b+1;
    }

    cost3 = max(cost3, best3);
    cost2 = max(cost2, best2);
    mpz_set(n.z, best.z);
  }

  double res = cost3*costs.cube + cost2*costs.square;
  if (term_count > 1)
    res += (term_count-1)*costs.compose;
  return res;
}

/**
 * Uses a greedy approach whereby it computes numbers of the form
 * N'=|(N - 2^a3^b)/(2^c3^d)| such that N' is minimal assuming that N is positive
 * and 0<=a<=max_a, 0<=b<=max_b, 0<=c<=max_c, 0<=d<=max_d
 */
double cost_subreduce(const group_cost_t& costs, const mpz_t in_n, int max_a, int max_b) {
  static mpz_c best_d;
  static mpz_c v;
  int a = 0;
  int b = 0;
  int A = 0;
  int B = 0;
  int reduce_a = 0;
  int reduce_b = 0;
  int best_a = 0;
  int best_b = 0;
  int best_reduce_a = 0;
  int best_reduce_b = 0;
  int outer_a = 0;
  int outer_b = 0;
  int cost_a = 0;
  int cost_b = 0;
  int term_count = 0;

  mpz_c n(in_n);
  mpz_c t;

  while (mpz_cmpabs_ui(n.z, 1) > 0) {
    mpz_set(best_d.z, n.z);

    B = mpz_sizeinbase(n.z, 3);
    if (B > max_b-outer_b) B = max_b-outer_b;
        
    for (b = 0;  b <= B;  b ++) {
      // compute initial value to +/-
      mpz_ui_pow_ui(v.z, 3, b);

      // compute upper bound for a
      mpz_tdiv_q(t.z, n.z, v.z);
      A = mpz_sizeinbase(t.z, 2);
      if (A > max_a-outer_a) A = max_a-outer_a;
            
      for (a = 0;  a <= A;  a ++, mpz_mul_2exp(v.z, v.z, 1)) {
	// compute N-2^a3^b
	mpz_sub(t.z, n.z, v.z);
                
	// quick reduce
	reduce_a = t.reduce2(max_a-outer_a);
	reduce_b = t.reduce3(max_b-outer_b);

	// check if this is the smallest value
	if (mpz_cmpabs(t.z, best_d.z) < 0) {
	  mpz_set(best_d.z, t.z);
	  best_a = a;
	  best_b = b;
	  best_reduce_a = reduce_a;
	  best_reduce_b = reduce_b;
	}

	// compute N+2^a3^b
	mpz_add(t.z, n.z, v.z);

	// quick reduce
	reduce_a = t.reduce2(max_a-outer_a);
	reduce_b = t.reduce3(max_b-outer_b);

	// check if this is the smallest value
	if (mpz_cmpabs(t.z, best_d.z) < 0) {
	  mpz_set(best_d.z, t.z);
	  best_a = a;
	  best_b = b;
	  best_reduce_a = reduce_a;
	  best_reduce_b = reduce_b;
	}
      }
    }

    // best_* represents the smallest new value of n and how to get there
    mpz_set(n.z, best_d.z);

    cost_a = max(cost_a, best_a + outer_a);
    cost_b = max(cost_b, best_b + outer_b);

    outer_a += best_reduce_a;
    outer_b += best_reduce_b;

    term_count ++;
  }

  return cost_a * costs.square +
         cost_b * costs.cube + 
         (term_count-1) * costs.compose;
}

double cost_subreduce(const group_cost_t& costs, const mpz_t in_n) {
  static mpz_c t;
  double best_cost = std::numeric_limits<double>::max();
  int k = mpz_sizeinbase(in_n, 3);
  int max_a = 0;
  int max_b = 0;

  while (max_b <= k/10) {
    mpz_ui_pow_ui(t.z, 3, max_b);
    mpz_tdiv_q(t.z, in_n, t.z);
    max_a = mpz_sizeinbase(t.z, 2);

    double cost = cost_subreduce(costs, in_n, max_a, max_b);
    if (cost < best_cost) {
      best_cost = cost;
    }

    max_b ++;
  }

  return best_cost;
}

double cost_pow_dbns_pre_block(const group_cost_t& costs,
			       const factored_two_three_term16_t reps[65536],
			       const int rep_sizes[65536],
			       const mpz_t n) {
  const uint16_t* blockp;
  int blocks;
  double res = 0;
  const factored_two_three_term16_t* rep;
  int rep_size;
  int i;
  int max_a = 0;
  int a;

  // set pointer
  blockp = (const uint16_t*)n->_mp_d;

  // compute how many 16bit blocks to do
  blocks = n->_mp_size * (GMP_LIMB_BITS / 16);

  for (; blocks > 0; blocks --, blockp ++) {
    rep = &reps[*blockp];
    rep_size = rep_sizes[*blockp];

    // add the cost for the stored squares
    if (blocks > 1) {
      res += costs.square * 16;
    } else {
      // only square as many as are needed
      // find the highest 'a' in the representation
      max_a = 0;
      for (i = 0;  i < rep_size;  i ++) {
	a = rep[i].a & 0x7F;
	if (a > max_a) {
	  max_a = a;
	}
      }
      res += costs.square * max_a;
    }

    // cost of all cubings
    for (i = 0;  i < rep_size;  i ++) {
      res += costs.cube * rep[i].b;
    }

    // number of composes is terms-1
    if (rep_size > 1) {
      res += costs.compose * (rep_size - 1);
    }
  }

  return res;
}

double cost_pow_dbns_pre_u16(const group_cost_t& costs,
			     const factored_two_three_term16_t reps[65536],
			     const int* rep_sizes,
			     const uint16_t n) {
  double res = 0;
  const factored_two_three_term16_t* rep;
  int rep_size;
  int i;
  int max_a = 0;
  int a;

  rep = &reps[n];
  rep_size = rep_sizes[n];

  // find the highest 'a' in the representation
  max_a = 0;
  for (i = 0;  i < rep_size;  i ++) {
    a = rep[i].a & 0x7F;
    if (a > max_a) {
      max_a = a;
    }
  }
  res += costs.square * max_a;

  // cost of all cubings
  for (i = 0;  i < rep_size;  i ++) {
    res += costs.cube * rep[i].b;
  }

  // number of composes is terms-1
  if (rep_size > 1) {
    res += costs.compose * (rep_size - 1);
  }
  return res;
}

double cost_pow_dbns_pre_list(const group_cost_t& costs,
			      const factored_two_three_term16_t reps[65536],
			      const int rep_sizes[65536],
			      const uint32_t* list,
			      const int list_size) {
  double res = 0;
  for (int i = 0;  i < list_size;  i ++) {
    res += cost_pow_dbns_pre_u16(costs, reps, rep_sizes, list[i]);
  }
  return res;
}

/**
 * Compute |(N-2^a3^b)/(2^c3^d) for 0 \le a \le A, 0 \le b \le B and reduce by 2,3
 * Keep the smallest branches and repeat.
 */
double cost_prune_subreduce(const group_cost_t& costs,
			    const mpz_t x,
			    int branch_count,
			    int max_a,
			    int max_b) {
  static mpz_c v;
  prune_c t;
  set<prune_c>::const_iterator branch;
  set<prune_c> branches;
  set<prune_c> new_branches;
  set<prune_c>::const_iterator iter;
  int i;
  int reduce_a;
  int reduce_b;

  prune_c::costs = &costs;
  prune_c::term_count = 1;

  branches.insert(prune_c(x));

  while (true) {
    // increment term count
    prune_c::term_count ++;

    // generate new branches
    new_branches.clear();
    for (branch = branches.begin();
	 branch != branches.end();
	 branch ++
	 ) {
      int B = mpz_sizeinbase(branch->z, 3);
      if (B > max_b-branch->outer_b) B = max_b-branch->outer_b;

      for (int b = 0;  b <= B;  b ++) {
	// compute initial value
	mpz_ui_pow_ui(v.z, 3, b);

	// compute bound
	mpz_tdiv_q(t.z, branch->z, v.z);
	int A = mpz_sizeinbase(t.z, 2);
	if (A > max_a-branch->outer_a) A = max_a-branch->outer_a;

	for (int a = 0;  a <= A;  a ++, mpz_mul_2exp(v.z, v.z, 1)) {
	  // branch->z - v
	  mpz_sub(t.z, branch->z, v.z);
	  reduce_a = t.reduce2(max_a-branch->outer_a);
	  reduce_b = t.reduce3(max_b-branch->outer_b);
	  t.cost_a = max(branch->cost_a, a + branch->outer_a);
	  t.cost_b = max(branch->cost_b, b + branch->outer_b);
	  t.outer_a = branch->outer_a + reduce_a;
	  t.outer_b = branch->outer_b + reduce_b;
	  new_branches.insert(t);
	  iter = new_branches.begin();
	  i = 0;
	  mpz_set_ui(t.z, 0);
	  while (iter != new_branches.end() && i < branch_count) {
	    if (mpz_cmp(iter->z, t.z) != 0) {
	      mpz_set(t.z, iter->z);
	      iter ++;
	    } else {
	      new_branches.erase(iter++);
	    }
	    i ++;
	  }
	  while (iter != new_branches.end()) {
	    new_branches.erase(iter++);
	  }

	  // branch->z + v
	  mpz_add(t.z, branch->z, v.z);
	  reduce_a = t.reduce2(max_a-branch->outer_a);
	  reduce_b = t.reduce3(max_b-branch->outer_b);
	  t.cost_a = max(branch->cost_a, a + branch->outer_a);
	  t.cost_b = max(branch->cost_b, b + branch->outer_b);
	  t.outer_a = branch->outer_a + reduce_a;
	  t.outer_b = branch->outer_b + reduce_b;
	  new_branches.insert(t);
	  iter = new_branches.begin();
	  i = 0;
	  mpz_set_ui(t.z, 0);
	  while (iter != new_branches.end() && i < branch_count) {
	    if (mpz_cmp(iter->z, t.z) != 0) {
	      mpz_set(t.z, iter->z);
	      iter ++;
	    } else {
	      new_branches.erase(iter++);
	    }
	    i ++;
	  }
	  while (iter != new_branches.end()) {
	    new_branches.erase(iter++);
	  }
	}
      }
    }

    // prune branches
    branches.clear();
    for (set<prune_c>::const_iterator i = new_branches.begin();  i != new_branches.end();  i ++) {
      if (mpz_cmpabs_ui(i->z, 1) == 0) {
	return i->cost();
      }
      branches.insert(*i);
    }
  }
  return 0;
}

double cost_prune_subreduce(const group_cost_t& costs, const mpz_t in_n) {
  static mpz_c t;
  double best_cost = std::numeric_limits<double>::max();
  int k = mpz_sizeinbase(in_n, 3);
  int max_b = 0;
  int max_a = 0;

  while (max_b <= k/10) {
    mpz_ui_pow_ui(t.z, 3, max_b);
    mpz_tdiv_q(t.z, in_n, t.z);
    max_a = mpz_sizeinbase(t.z, 2);

    double cost = cost_prune_subreduce(costs, in_n, 16, max_a, max_b);
    if (cost < best_cost) {
      best_cost = cost;
    }

    max_b ++;
  }
  return best_cost;
}

/**
 * Compute |(N \pm 1)/2^a3^b|
 * Keep the smallest branches and repeat.
 */
double cost_prune_pm1(const group_cost_t& costs, const mpz_t x, int branch_count = 16) {
  static mpz_c v;
  static prune_c t;
  static set<prune_c>::const_iterator branch;
  static set<prune_c> branches;
  static set<prune_c> new_branches;
  static set<prune_c>::const_iterator iter;
  int i;
  int reduce_a;
  int reduce_b;

  prune_c::costs = &costs;
  prune_c::term_count = 0;

  branches.clear();
  branches.insert(prune_c(x));

  while (true) {
    // increment term count
    prune_c::term_count ++;

    // generate new branches
    new_branches.clear();
    for (branch = branches.begin();
	 branch != branches.end();
	 branch ++
	 ) {
      if (is_2_3_integer(branch->z, &reduce_a, &reduce_b)) {
	t.cost_a = branch->cost_a + reduce_a;
	t.cost_b = branch->cost_b + reduce_b;
	mpz_set_ui(t.z, 1);
	new_branches.insert(t);
      } else {
	// N-1
	mpz_sub_ui(t.z, branch->z, 1);
	reduce_a = t.reduce2();
	reduce_b = t.reduce3();
	t.cost_a = branch->cost_a + reduce_a;
	t.cost_b = branch->cost_b + reduce_b;
	new_branches.insert(t);

	// N+1
	mpz_add_ui(t.z, branch->z, 1);
	reduce_a = t.reduce2();
	reduce_b = t.reduce3();
	t.cost_a = branch->cost_a + reduce_a;
	t.cost_b = branch->cost_b + reduce_b;
	new_branches.insert(t);
      }

      // only keep 'best' branches
      // lowest branches, if two branches are equally low, use cheaper branch
      iter = new_branches.begin();
      i = 0;
      mpz_set_ui(t.z, 0);
      while (iter != new_branches.end() && i < branch_count) {
	if (mpz_cmp(iter->z, t.z) != 0) {
	  mpz_set(t.z, iter->z);
	  iter ++;
	  i ++;
	} else {
	  new_branches.erase(iter++);
	}
      }
      while (iter != new_branches.end()) {
	new_branches.erase(iter++);
      }
    }

    // prune branches
    branches.clear();
    for (iter = new_branches.begin();  iter != new_branches.end();  iter ++) {
      if (mpz_cmpabs_ui(iter->z, 1) <= 0) {
	return iter->cost();
      }
      branches.insert(*iter);
    }
  }

  return 0;
}

/**
 * Compute |(N - 2^a3^b)| for 0 \le a \le A, 0 \le b \le B
 * Keep the smallest branches and repeat.
 */
double cost_prune_closest(const group_cost_t& costs,
			  const mpz_t x,
			  int branch_count,
			  int max_a, int max_b) {
  static mpz_c v;
  static prune_c t;
  static set<prune_c>::const_iterator branch;
  static set<prune_c> branches;
  static set<prune_c> new_branches;
  static set<prune_c>::const_iterator iter;
  int i;

  prune_c::costs = &costs;
  prune_c::term_count = 0;

  branches.clear();
  branches.insert(prune_c(x));

  while (true) {
    // increment term count
    prune_c::term_count ++;

    // generate new branches
    new_branches.clear();
    for (branch = branches.begin();
	 branch != branches.end();
	 branch ++
	 ) {
      int a;
      int b;
      if (is_2_3_integer(branch->z, &a, &b)) {
	t.cost_a = max(branch->cost_a, a);
	t.cost_b = max(branch->cost_b, b);
	return t.cost();
      }

      int B = mpz_sizeinbase(branch->z, 3);
      if (B > max_b) B = max_b;

      // compute initial value (v = 2^a <= branch)
      a = mpz_sizeinbase(branch->z, 2)-1;
      if (a > max_a) a = max_a;
      mpz_set_ui(v.z, 1);
      mpz_mul_2exp(v.z, v.z, a);

      b = 0;
      while (b <= B) {
	// positive or negative?
	if (branch->z->_mp_size < 0) {
	  // negative
	  mpz_add(t.z, branch->z, v.z);
	} else {
	  // positive
	  mpz_sub(t.z, branch->z, v.z);
	}
	t.cost_a = max(branch->cost_a, a);
	t.cost_b = max(branch->cost_b, b);
	// insert new branch
	new_branches.insert(t);

	// positive or negative using 2v?
	if (branch->z->_mp_size < 0) {
	  // negative
	  mpz_add(t.z, branch->z, v.z);
	  mpz_add(t.z, t.z, v.z);
	} else {
	  // positive
	  mpz_sub(t.z, branch->z, v.z);
	  mpz_sub(t.z, t.z, v.z);
	}
	t.cost_a = max(branch->cost_a, a + 1);
	t.cost_b = max(branch->cost_b, b);
	// insert new branch
	new_branches.insert(t);
                
	// only keep 'best' branches
	// lowest branches, if two branches are equally low, use cheaper branch
	iter = new_branches.begin();
	i = 0;
	mpz_set_ui(t.z, 0);
	while (iter != new_branches.end() && i < branch_count) {
	  if (mpz_cmp(iter->z, t.z) != 0) {
	    mpz_set(t.z, iter->z);
	    iter ++;
	    i ++;
	  } else {
	    new_branches.erase(iter++);
	  }
	}
	while (iter != new_branches.end()) {
	  new_branches.erase(iter++);
	}

	// next value, increment b, reduce 'a' while v > branch
	mpz_mul_ui(v.z, v.z, 3);
	b ++;
	while (mpz_cmpabs(v.z, branch->z) > 0 && a >= 0) {
	  mpz_tdiv_q_2exp(v.z, v.z, 1);
	  a --;
	}
	if (a < 0) {
	  break;
	}
      }
    }

    // prune branches
    branches.clear();
    for (iter = new_branches.begin();  iter != new_branches.end();  iter ++) {
      if (mpz_cmpabs_ui(iter->z, 1) <= 0) {
	return iter->cost();
      }
      branches.insert(*iter);
    }
  }
  return 0;
}

/// Generate the output filename.
string dat_file(const string& type, const string& ext) {
  return "dat/" + type + "-" + ext + ".dat";
}

void time_primorial_growth(const group_cost_t& costs,
			   const string& type,
			   const string& ext,
			   const ICostExp& cost_exp) {
  cout << setprecision(5) << fixed;
  uint32_t* primes = first_n_primes(prime_count);
  primes[0] = 1;  // We only want odd primes.
  mpz_c primorial(1);
  int prime_index = 0;
  const string out_file = dat_file(type, ext);
  remove(out_file.c_str());

  while (prime_index < prime_count) {
    // Multiply in the next prime.
    for (int i = prime_step;
	 i > 0 && prime_index < prime_count;
	 i--, prime_index++) {
      mpz_mul_ui(primorial.z, primorial.z, primes[prime_index]);
    }

    cout << "Using the first " << prime_index
         << " odd primes on a " << ext << "-bit discriminant." << endl;
    int primorial_size = mpz_sizeinbase(primorial.z, 2);
    cout << "Primorial has " << primorial_size << " bits." << endl;

    // Compute time of function.
    double c = cost_exp.cost(costs, primorial);
    cout << type << ": " << c << endl;
    append_gnuplot_datfile(out_file, prime_index, c);
    cout << endl;
  }
  free(primes);
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
  const string out_file = dat_file(type, ext);
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
  CostNafR2L         cost_naf_r2l;
  CostDBNSChainR2L   cost_dbns_r2l;
  CostDBNSChainR2L36 cost_dbns_r2l36;
  Cost_DBNS_L2R      cost_dbns_l2r;
  CostGreedyPM1      cost_greedy_pm1_tree(16);
  CostClosest23Tree  cost_closest_23_tree(16);
  const fnc_desc descs[] = {
    //    {"binary", cost_binary},
    //    {"block", cost_block},
    //    {"naf_r2l", cost_naf_r2l},
    //    {"dbns_r2l", cost_dbns_r2l},
    //    {"dbns_r2l36", cost_dbns_r2l36},
    //    {"dbns_l2r", cost_dbns_l2r},
    {"greedy_pm1_tree", cost_greedy_pm1_tree},
    //    {"closest_23_tree", cost_closest_23_tree},
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
	     "greedy_pm1_tree_65536", "64", CostGreedyPM1(16),
	     1, 65535, 1);
  time_range(s128_qform_costs,
	     "greedy_pm1_tree_65536", "128", CostGreedyPM1(16),
	     1, 65535, 1);
}


int main(int argc, char** argv) {
  struct rlimit l = {1024ULL*1024ULL*1024ULL, 1024ULL*1024ULL*1024ULL};
  setrlimit(RLIMIT_AS, &l);

  time_methods();
  //  time_16bit_methods();

  //graph_dbns_l2r_bounds(s64_qform_costs, 1000,
  //  			dat_file("dbns_l2r_bounded", "64"));
  //graph_dbns_l2r_bounds(s128_qform_costs, 1000,
  //                    dat_file("dbns_l2r_bounded", "128"));

  /*
  for (prime_count = 100; prime_count <= 5000; prime_count += 100) {
    prime_step = prime_count;
    cost_to_mask.clear();
    cout << setprecision(5) << fixed;
    for (dbns_mask = 0; dbns_mask < 4096; dbns_mask++) {
      time_primorial_growth(s64_qform_costs,
			    s64_pow_reps, s64_pow_rep_sizes,
			    "out", "64", &cost_weird_dbns);
    }
    //    auto iter = cost_to_mask.begin();
    //    for (int i = 0; i < 100; i++) {
    //      cout << "mask=" << iter->second << " cost=" << iter->first << endl;
    //      iter++;
    //    }
    auto iter = cost_to_mask.begin();
    cout << "count=" << prime_count << ' '
	 << "mask=" << iter->second << endl;
  }
  */
  return 0;
}


