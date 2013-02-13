#pragma once
#ifndef COST_DBNS_CHAIN_R2L_H_
#define COST_DBNS_CHAIN_R2L_H_

#include "powcosts/i_cost_exp.h"

class CostDBNSChainR2L : public ICostExp {
 public:
  double cost(const group_cost_t& cost, const mpz_c& in_n) const;
};

class CostDBNSChainR2L36 : public ICostExp {
 public:
  double cost(const group_cost_t& costs, const mpz_c& in_n) const;
};

class CostDBNSChainR2L36Prog : public ICostExp {
 public:
  CostDBNSChainR2L36Prog(const int mask) : mask_(mask) {}
  double cost(const group_cost_t& costs, const mpz_c& in_n) const;
 private:
  int mask_;
};

#endif

