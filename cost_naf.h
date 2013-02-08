#pragma once
#ifndef COST_NAF_H_
#define COST_NAF_H_

#include <gmp.h>

#include "powcosts/mpz_c.h"

extern "C" {
#include "liboptarith/group.h"
}

double cost_naf_r2l(const group_cost_t& cost, const mpz_c& in_n);

#endif

