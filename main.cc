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
#include <map>
#include <set>
#include <sstream>
#include <vector>

#include <float.h>
#include <gmp.h>
#define __STDC_LIMIT_MACROS
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>

#include "term.h"
#include "mpz_c.h"
#include "prune_c.h"
#include "rep_c.h"

#include "powcosts/cost_binary.h"
#include "powcosts/cost_dbns_chain_r2l.h"
#include "powcosts/cost_naf.h"

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

/// Write a gnuplot data file
void write_gnuplot_datfile(const char* filename,
			   const uint64_t* x,
			   const double* y,
			   const int sample_count) {
  ofstream f(filename);
  f << setprecision(5);
  for (int i = 0;  i < sample_count;  i ++) {
    f << x[i] << ", " << y[i] << endl;
  }
  f.close();
}

/// Append a row to a gnuplot data file.
void append_gnuplot_datfile(const string& filename,
			    const uint64_t x,
			    const double y) {
  ofstream f;
  f.open(filename.c_str(), fstream::out | fstream::app);
  f << setprecision(5);
  f << x << ", " << y << endl;
  f.close();
}

/// \f$ v = 2^a * 3^b \f$
inline void two_three_val(mpz_t v, const int a, const int b) {
  mpz_ui_pow_ui(v, 3, b);
  mpz_mul_2exp(v, v, a);
}

/**
 * True if x is of the form \f$ s2^a3^b \f$ where s in {1,-1}.
 * a and b are output paramters.
 */
bool is_2_3_integer(const mpz_t x, int* a, int* b) {
  mpz_c t;

  // make sure x is not zero
  if (mpz_cmp_ui(x, 0) == 0) {
    (*a) = 0;
    (*b) = 0;
    return false;
  }

  // remove powers of 2
  (*a) = mpz_scan1(x, 0);
  if (*a > 0) {
    mpz_tdiv_q_2exp(t.z, x, *a);
  } else {
    mpz_set(t.z, x);
  }

  // remove powers of 3
  (*b) = 0;
  while (mpz_mod3(t.z) == 0) {
    mpz_divexact_ui(t.z, t.z, 3);
    (*b) ++;
  }

  return (mpz_cmpabs_ui(t.z, 1) == 0);
}

/**
 * Computes a 2^a3^b such that the distance from n is minimal.
 * Returns the value of the best approximation (2^a3^b).
 */
mpz_c best_db_approx(int* out_a,
		     int* out_b,
		     const mpz_t n,
		     const int max_a,
		     const int max_b) {
  if (is_2_3_integer(n, out_a, out_b)) {
    return n;
  }

  mpz_c approx;
  mpz_c best_d;
  mpz_c d;
  mpz_c v;
  mpz_c tmp_v;
  int best_a = max_a;
  int best_b = max_b;
  int a;
  int b;
  int tmp;

  mpz_set(best_d.z, n);

  // find largest 'a' such that 2^a <= n
  a = mpz_sizeinbase(n, 2) - 1;
  if (a > max_a) a = max_a;
  mpz_set_ui(v.z, 1);
  mpz_mul_2exp(v.z, v.z, a);

  // find largest 'b' such that 2^a3^b <= n
  b = 0;
  mpz_set(tmp_v.z, v.z);
  while (mpz_cmp(tmp_v.z, n) <= 0) {
    mpz_set(v.z, tmp_v.z);

    // tmp_v = v*3
    mpz_mul_2exp(tmp_v.z, v.z, 1);
    mpz_add(tmp_v.z, tmp_v.z, v.z);
        
    b ++;
  }
  b --;

  // iterate all values of 'a'
  while (a >= 0 && b <= max_b) {
    // check 2^a*3^b
    mpz_sub(d.z, n, v.z);
    tmp = mpz_cmpabs(d.z, best_d.z);
    if (tmp < 0) {
      mpz_set(approx.z, v.z);
      mpz_set(best_d.z, d.z);
      best_a = a;
      best_b = b;
    }

    // check 2^{a+1}*3^b
    if (a+1 <= max_a) {
      mpz_sub(d.z, d.z, v.z);
      tmp = mpz_cmpabs(d.z, best_d.z);
      if (tmp < 0) {
	mpz_mul_2exp(approx.z, v.z, 1);
	mpz_set(best_d.z, d.z);
	best_a = a+1;
	best_b = b;
      }
    }

    // next 'a'
    a --;
    mpz_tdiv_q_2exp(v.z, v.z, 1);

    // find largest 'b' such that 2^a3^b <= n
    mpz_set(tmp_v.z, v.z);
    while (mpz_cmp(tmp_v.z, n) <= 0) {
      mpz_set(v.z, tmp_v.z);

      // tmp_v = v*3
      mpz_mul_2exp(tmp_v.z, v.z, 1);
      mpz_add(tmp_v.z, tmp_v.z, v.z);
      b ++;
    }
    b --;
  }

  *out_a = best_a;
  *out_b = best_b;
  return approx;
}

/**
 * Exponentiates using a DB-representation from left-to-right
 */
double cost_pow_dbns_l2r(const group_cost_t& costs,
			 const mpz_t in_n,
			 const int max_a, const int max_b) {
  mpz_c n(in_n);
  mpz_c t;
  double res = 0;
  int cost_a = 0;
  int cost_b = 0;
  int a;
  int b;
  int terms = 0;

  while (mpz_cmp_ui(n.z, 0) != 0) {
    terms ++;

    if (mpz_cmp_ui(n.z, 0) > 0) {
      mpz_c approx = best_db_approx(&a, &b, n.z, max_a, max_b);
      if (a > cost_a) cost_a = a;
      if (b > cost_b) cost_b = b;
      mpz_sub(n.z, n.z, approx.z);
    } else {
      mpz_abs(t.z, n.z);
      mpz_c approx = best_db_approx(&a, &b, t.z, max_a, max_b);
      if (a > cost_a) cost_a = a;
      if (b > cost_b) cost_b = b;
      mpz_add(n.z, n.z, approx.z);
    }
  }

  res = 0;
  res += cost_a * costs.square;
  res += cost_b * costs.cube;
  if (terms > 1) {
    res += (terms-1) * costs.compose;
  }

  return res;
}


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
 * Exponentiates using a DB-representation from left-to-right
 * Timing tests show that the high 10% of 'max_a' seems to be sufficient
 * given our costs on quadratic forms.
 */
double cost_pow_dbns_l2r(const group_cost_t& costs, const mpz_t in_n) {
  static mpz_c t;
  double best_cost = FLT_MAX;
  int k = mpz_sizeinbase(in_n, 3);
  int max_a = 0;
  int max_b = 0;

  while (max_b <= k) {
    mpz_ui_pow_ui(t.z, 3, max_b);
    mpz_tdiv_q(t.z, in_n, t.z);
    max_a = mpz_sizeinbase(t.z, 2);

    double cost = cost_pow_dbns_l2r(costs, in_n, max_a, max_b);
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
double cost_pow_dbns_chain_l2r(const group_cost_t& costs, const mpz_t in_n, const int in_max_a, const int in_max_b) {
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
double cost_pow_dbns_chain_l2r(const group_cost_t& costs, const mpz_t in_n) {
  static mpz_c t;
  double best_cost = FLT_MAX;
  int k = mpz_sizeinbase(in_n, 2);
  int max_a = 9*k/10;
  int max_b;

  while (max_a <= k) {
    mpz_tdiv_q_2exp(t.z, in_n, max_a);
    max_b = mpz_sizeinbase(t.z, 3);

    double cost = cost_pow_dbns_chain_l2r(costs, in_n, max_a, max_b);
    if (cost < best_cost) best_cost = cost;

    max_a ++;
  }

  return best_cost;
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
  double best_cost = FLT_MAX;
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

double cost_strict_chain(const group_cost_t& costs, const mpz_c x) {
  if (mpz_cmp_ui(x.z, 1) == 0)
    return 0;
  if (mpz_cmp_ui(x.z, 2) == 0)
    return costs.square;
  if (mpz_cmp_ui(x.z, 3) == 0)
    return costs.cube;

  vector<mpz_c> values;
  values.push_back(x);

  map<mpz_c, double> mem;
  mem[mpz_c(1)] = 0;
  mem[mpz_c(2)] = costs.square;
  mem[mpz_c(3)] = costs.cube;

  mpz_c t1;
  mpz_c t2;
  map<mpz_c, double>::const_iterator t1_iter;
  map<mpz_c, double>::const_iterator t2_iter;

  while (values.size() > 0) {
    mpz_c cur = values.back();
    if (mem.find(cur) != mem.end()) {
      values.pop_back();
      continue;
    }

    // let r = x mod 6
    t1.div2(cur);
    int r = (mpz_mod3(t1.z) << 1) | (cur.z->_mp_d[0]&1);
    switch (r) {
    case 0:
      // min ( s(cur/3), s(cur/2) )
      t1.div3(cur);
      t2.div2(cur);
      t1_iter = mem.find(t1);
      t2_iter = mem.find(t2);
      if (t1_iter != mem.end() && t2_iter != mem.end()) {
	values.pop_back();
	mem[cur] = min(costs.cube + t1_iter->second, costs.square + t2_iter->second);
      } else {
	// compute each of the intermediates
	values.push_back(t1);
	values.push_back(t2);
      }
      break;

    case 1:
      // 1 + s(cur-1)
      mpz_sub_ui(t1.z, cur.z, 1);
      t1_iter = mem.find(t1);
      if (t1_iter != mem.end()) {
	values.pop_back();
	mem[cur] = costs.compose + t1_iter->second;
      } else {
	values.push_back(t1);
      }
      break;

    case 2:
      // s(cur/2)
      t1.div2(cur);
      t1_iter = mem.find(t1);
      if (t1_iter != mem.end()) {
	values.pop_back();
	mem[cur] = costs.square + t1_iter->second;
      } else {
	values.push_back(t1);
      }
      break;

    case 3:
      // min ( s(cur/3), 1+s((cur-1)/2) )
      mpz_sub_ui(t1.z, cur.z, 1);
      t2.div2(t1);
      t1.div3(cur);

      t1_iter = mem.find(t1);
      t2_iter = mem.find(t2);
      if (t1_iter != mem.end() && t2_iter != mem.end()) {
	values.pop_back();
	mem[cur] = min(costs.cube + t1_iter->second, costs.square + costs.compose + t2_iter->second);
      } else {
	values.push_back(t1);
	values.push_back(t2);
      }
      break;

    case 4:
      // min ( s(cur/2), s((cur-1)/3) )
      mpz_sub_ui(t1.z, cur.z, 1);
      t2.div3(t1);
      t1.div2(cur);

      t1_iter = mem.find(t1);
      t2_iter = mem.find(t2);
      if (t1_iter != mem.end() && t2_iter != mem.end()) {
	values.pop_back();
	mem[cur] = min(costs.square + t1_iter->second, costs.cube + costs.compose + t2_iter->second);
      } else {
	values.push_back(t1);
	values.push_back(t2);
      }
      break;

    case 5:
      // 1 + s((cur-1)/2)
      mpz_sub_ui(t1.z, cur.z, 1);
      t1.div2(t1);

      t1_iter = mem.find(t1);
      if (t1_iter != mem.end()) {
	values.pop_back();
	mem[cur] = costs.square + costs.compose + t1_iter->second;
      } else {
	values.push_back(t1);
      }
      break;
    }
  }
  return mem[x];
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
  double best_cost = FLT_MAX;
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

/**
 * Exponentiates using a DB-representation from left-to-right
 */
void graph_pow_dbns_l2r(const group_cost_t& costs,
			const mpz_t in_n,
			const char* filename) {
  static mpz_c t;
  ofstream f;
  int k = mpz_sizeinbase(in_n, 2);
  int max_a = 0;
  int max_b;
  double best_cost = FLT_MAX;
  int best_max_a = 0;
  int best_max_b = 0;

  f.open(filename);
  while (max_a <= k) {
    cout << "\r" << (max_a*100.0/k) << "%                                            " << flush;

    mpz_tdiv_q_2exp(t.z, in_n, max_a);
    max_b = mpz_sizeinbase(t.z, 3);

    double cost = cost_pow_dbns_l2r(costs, in_n, max_a, max_b);
    if (cost < best_cost) {
      best_cost = cost;
      best_max_a = max_a;
      best_max_b = max_b;
    }

    f << max_a << ", " << cost << endl;

    max_a ++;
  }
  cout << "\r                                            \r" << flush;
  f.close();

  cout << filename << ": " << best_max_a << " " << best_max_b << endl;
}

/// Generate the output filename.
string dat_file(const string& type, const string& ext) {
  return "dat/" + type + "-" + ext + ".dat";
}

void time_primorial_growth(const group_cost_t& costs,
			   factored_two_three_term16_t* reps[65536],
			   const int rep_sizes[65536],
			   const string& ext) {
  // Remove dat files.
  remove(dat_file("binary", ext).c_str());
  remove(dat_file("naf_r2l", ext).c_str());
  remove(dat_file("dbns_chain_r2l", ext).c_str());
  remove(dat_file("dbns_chain_r2l36", ext).c_str());

  // Generate list of primes.
  uint32_t* primes = first_n_primes(prime_count);
  mpz_c primorial(1);
  double c;

  // Iterate over primorials.
  for (int n = 1; n < prime_count; n++) {
    cout << "Using the first " << n << " odd primes." << endl;

    // Multiply in the next prime.
    mpz_mul_ui(primorial.z, primorial.z, primes[n]);
    int primorial_size = mpz_sizeinbase(primorial.z, 2);
    cout << "Primorial has " << primorial_size << " bits." << endl;

    // binary.
    c = cost_binary(costs, primorial);
    cout << "Binary: " << c << endl;
    append_gnuplot_datfile(dat_file("binary", ext), n, c);

    // NAF R2L.
    c = cost_naf_r2l(costs, primorial);
    cout << "NAF R2L: " << c << endl;
    append_gnuplot_datfile(dat_file("naf_r2l", ext), n, c);

    // DBNS Chain R2L
    c = cost_dbns_chain_r2l(costs, primorial);
    cout << "DBNS Chain R2L: " << c << endl;
    append_gnuplot_datfile(dat_file("dbns_chain_r2l", ext), n, c);

    // DBNS Chain R2L (mod 36)
    c = cost_dbns_chain_r2l36(costs, primorial);
    cout << "DBNS Chain R2L (mod 36): " << c << endl;
    append_gnuplot_datfile(dat_file("dbns_chain_r2l36", ext), n, c);

    cout << endl;
  }
  free(primes);
}

int main(int argc, char** argv) {
  time_primorial_growth(s64_qform_costs,
			s64_pow_reps, s64_pow_rep_sizes, "64");
  time_primorial_growth(s128_qform_costs,
			s128_pow_reps, s128_pow_rep_sizes, "128");
  return 0;
}


