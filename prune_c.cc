#include "prune_c.h"

extern "C" {
#include "liboptarith/group.h"
}

int prune_c::term_count = 0;
const group_cost_t* prune_c::costs = &compose_only_costs;
