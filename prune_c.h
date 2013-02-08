#pragma once
#ifndef PRUNE_C__INCLUDED
#define PRUNE_C__INCLUDED

#include <gmp.h>
#include "mpz_c.h"

extern "C" {
#include "liboptarith/group.h"
}

class prune_c : public mpz_c {
 public:
  prune_c()
    : mpz_c()
    , cost_a(0)
    , cost_b(0)
    , outer_a(0)
    , outer_b(0) {
  }

  explicit prune_c(const mpz_t x)
    : mpz_c(x)
    , cost_a(0)
    , cost_b(0)
    , outer_a(0)
    , outer_b(0) {
  }

 prune_c(const prune_c& that)
   : mpz_c(that.z) {
    cost_a = that.cost_a;
    cost_b = that.cost_b;
    outer_a = that.outer_a;
    outer_b = that.outer_b;
  }

  prune_c& operator=(const prune_c& that) {
    mpz_set(z, that.z);
    cost_a = that.cost_a;
    cost_b = that.cost_b;
    outer_a = that.outer_a;
    outer_b = that.outer_b;
    return *this;
  }
  
  virtual double cost() const {
    double res = cost_a*costs->square + cost_b*costs->cube;
    if (term_count > 1)
      res += (term_count-1)*costs->compose;
    return res;
  }
  
  bool operator<(const prune_c& that) const {
    int res = mpz_cmpabs(z, that.z);
    if (res < 0) return true;
    if (res == 0) {
      if (cost() < that.cost()) return true;
    }
    return false;
  }
  
  int cost_a;  // squares
  int cost_b;  // cubes
  int outer_a;
  int outer_b;
  
  static const group_cost_t* costs;
  static int term_count;
};

#endif

