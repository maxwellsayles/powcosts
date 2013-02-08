#pragma once
#ifndef COST_CLOSEST_23_TREE_H_
#define COST_CLOSEST_23_TREE_H_

#include <gmp.h>

#include "powcosts/mpz_c.h"

extern "C" {
#include "liboptarith/group.h"
}

double cost_closest_23_tree(const group_cost_t& cost, const mpz_c& in_n);

#endif

