/**
 * @file partial_cost.h
 * Represents the cost to partially reduce an integer n.
 * Contains the number of squares, cubes, and terms so far,
 * as well as the value remaining to be reduced.
 */
#pragma once
#ifndef PARTIAL_COST_H_
#define PARTIAL_COST_H_

#include <memory>

#include "liboptarith/group.h"
#include "powcosts/mpz_c.h"

struct PartialCost {
 public:
  PartialCost(const group_cost_t& costs_, const mpz_c& n)
    : costs(costs_)
    , terms(0)
    , squares(0)
    , cubes(0)
    , remainder(new mpz_c(n))
  {
  }

  PartialCost(const group_cost_t& costs_,
	      std::shared_ptr<const mpz_c> n)
    : costs(costs_)
    , terms(0)
    , squares(0)
    , cubes(0)
    , remainder(n)
  {
  }

  PartialCost(const group_cost_t& costs_,
	      const int terms_, const int squares_, const int cubes_,
	      const mpz_c& remainder_)
    : costs(costs_)
    , terms(terms_)
    , squares(squares_)
    , cubes(cubes_)
    , remainder(new mpz_c(remainder_))
  {
  }

  PartialCost(const group_cost_t& costs_,
	      const int terms_, const int squares_, const int cubes_,
	      std::shared_ptr<const mpz_c> remainder_)
    : costs(costs_)
    , terms(terms_)
    , squares(squares_)
    , cubes(cubes_)
    , remainder(remainder_)
  {
  }

  PartialCost(const PartialCost& part)
    : costs(part.costs)
    , terms(part.terms)
    , squares(part.squares)
    , cubes(part.cubes)
    , remainder(part.remainder)
  {
  }

  /// Every member is const, so I don't see how assignment could be possible.
  PartialCost& operator=(const PartialCost& that);

  /// compute the cost incurred so far
  double partial_cost() const {
    return (terms - 1) * costs.compose +
	squares * costs.square + cubes * costs.cube;
  }

  /// Return true if the remainder is less,
  /// and if the remainders are equal, return true if the cost is less.
  bool operator<(const PartialCost& that) const {
    int res = mpz_cmpabs(remainder->z, that.remainder->z);
    if (res < 0) return true;
    if (res == 0) {
      return partial_cost() < that.partial_cost();
    }
    return false;
  }

  /// Return true if the remainder is greater,
  /// and if the remainders are equal, return true if the cost is greater.
  bool operator>(const PartialCost& that) const {
    int res = mpz_cmpabs(remainder->z, that.remainder->z);
    if (res > 0) return true;
    if (res == 0) {
      return partial_cost() > that.partial_cost();
    }
    return false;
  }

  const group_cost_t& costs;
  const int terms;
  const int squares;
  const int cubes;
  const std::shared_ptr<const mpz_c> remainder;
};

#endif

