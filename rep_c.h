#ifndef REP_C__INCLUDED
#define REP_C__INCLUDED

#include <vector>
#include "prune_c.h"
#include "term.h"

class rep_c : public prune_c {
public:
    rep_c() : prune_c() {}
    rep_c(const mpz_t x) : prune_c(x) {}
    rep_c(const rep_c& r) : prune_c(r), terms(r.terms.begin(), r.terms.end()) {}

    virtual double cost() const {
        return cost_a*costs->square + cost_b*costs->cube + (terms.size()-1)*costs->compose;
    }

    std::vector<term> terms;
};


#endif

