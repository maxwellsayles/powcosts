#pragma once
#ifndef COST_DBNS_CHAIN_R2L_H_
#define COST_DBNS_CHAIN_R2L_H_

#include <gmp.h>

#include "powcosts/mpz_c.h"

extern "C" {
#include "liboptarith/group.h"
}

double cost_dbns_chain_r2l(const group_cost_t& cost,
			   const mpz_c& in_n);

double cost_dbns_chain_r2l36(const group_cost_t& costs,
			     const mpz_c& in_n);


#endif

