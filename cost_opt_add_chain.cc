#include "powcosts/cost_opt_add_chain.h"

#include <map>
#include <vector>

using namespace std;

// TODO: We may need to test +/- 1 for some residues

double CostOptAddChain::cost(const group_cost_t& costs,
			     const mpz_c& x) const {
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

  while (!values.empty()) {
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
	mem[cur] = min(costs.cube + t1_iter->second,
		       costs.square + t2_iter->second);
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
	mem[cur] = min(costs.cube + t1_iter->second,
		       costs.square + costs.compose + t2_iter->second);
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
	mem[cur] = min(costs.square + t1_iter->second,
		       costs.cube + costs.compose + t2_iter->second);
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

