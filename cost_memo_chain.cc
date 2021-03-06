#include "powcosts/cost_memo_chain.h"

#include <map>
#include <vector>

using namespace std;

double CostMemoChain::cost(const group_cost_t& costs,
			   const mpz_c& x) const {
  vector<mpz_c> values;
  values.push_back(x);

  mem_[mpz_c(1)] = 0;
  mem_[mpz_c(2)] = costs.square;
  mem_[mpz_c(3)] = costs.cube;

  mpz_c t1;
  mpz_c t2;
  mpz_c t3;
  map<mpz_c, double>::const_iterator t1_iter;
  map<mpz_c, double>::const_iterator t2_iter;
  map<mpz_c, double>::const_iterator t3_iter;

  while (!values.empty()) {
    mpz_c cur = values.back();
    if (mem_.find(cur) != mem_.end()) {
      values.pop_back();
      continue;
    }

    // let r = x mod 6
    t1.div2(cur);
    int r = (mpz_mod3(t1.z) << 1) | (cur.z->_mp_d[0]&1);
    switch (r) {
    case 0:
      // min s(cur/3), s(cur/2)
      t1.div3(cur);
      t2.div2(cur);
      t1_iter = mem_.find(t1);
      t2_iter = mem_.find(t2);
      if (t1_iter != mem_.end() && t2_iter != mem_.end()) {
	values.pop_back();
	mem_[cur] = min(costs.cube + t1_iter->second,
			costs.square + t2_iter->second);
      } else {
	// compute each of the intermediates
	if (t1_iter == mem_.end()) values.push_back(t1);
	if (t2_iter == mem_.end()) values.push_back(t2);
      }
      break;

    case 1:
      // min s(cur-1), s((cur+1)/2)
      mpz_sub_ui(t1.z, cur.z, 1);
      mpz_add_ui(t2.z, cur.z, 1);
      t2.div2();
      t1_iter = mem_.find(t1);
      t2_iter = mem_.find(t2);
      if (t1_iter != mem_.end() && t2_iter != mem_.end()) {
	values.pop_back();
	mem_[cur] = min(costs.compose + t1_iter->second,
			costs.square + costs.compose + t2_iter->second);
      } else {
	if (t1_iter == mem_.end()) values.push_back(t1);
	if (t2_iter == mem_.end()) values.push_back(t2);
      }
      break;

    case 2:
      // min s(cur/2), s((cur+1)/3)
      t1.div2(cur);
      mpz_add_ui(t2.z, cur.z, 1);
      t2.div3();
      t1_iter = mem_.find(t1);
      t2_iter = mem_.find(t2);
      if (t1_iter != mem_.end() && t2_iter != mem_.end()) {
	values.pop_back();
	mem_[cur] = min(costs.square + t1_iter->second,
			costs.compose + costs.cube + t2_iter->second);
      } else {
	if (t1_iter == mem_.end()) values.push_back(t1);
	if (t2_iter == mem_.end()) values.push_back(t2);
      }
      break;

    case 3:
      // min s(cur/3), s((cur-1)/2), s((cur+1)/2)
      t1.div3(cur);
      mpz_sub_ui(t2.z, cur.z, 1);
      t2.div2();
      mpz_add_ui(t3.z, cur.z, 1);
      t3.div2();
      t1_iter = mem_.find(t1);
      t2_iter = mem_.find(t2);
      t3_iter = mem_.find(t3);
      if (t1_iter != mem_.end() &&
	  t2_iter != mem_.end() &&
	  t3_iter != mem_.end()) {
	values.pop_back();
	mem_[cur] = min(costs.cube + t1_iter->second,
			costs.square + costs.compose +
                            min(t2_iter->second, t3_iter->second));
                            
      } else {
	if (t1_iter == mem_.end()) values.push_back(t1);
	if (t2_iter == mem_.end()) values.push_back(t2);
	if (t3_iter == mem_.end()) values.push_back(t3);
      }
      break;

    case 4:
      // min s(cur/2), s((cur-1)/3)
      t1.div2(cur);
      mpz_sub_ui(t2.z, cur.z, 1);
      t2.div3();
      t1_iter = mem_.find(t1);
      t2_iter = mem_.find(t2);
      if (t1_iter != mem_.end() && t2_iter != mem_.end()) {
	values.pop_back();
	mem_[cur] = min(costs.square + t1_iter->second,
			costs.cube + costs.compose + t2_iter->second);
      } else {
	if (t1_iter == mem_.end()) values.push_back(t1);
	if (t2_iter == mem_.end()) values.push_back(t2);
      }
      break;

    case 5:
      // min s((cur-1)/2), s(cur+1)
      mpz_sub_ui(t1.z, cur.z, 1);
      t1.div2();
      mpz_add_ui(t2.z, cur.z, 1);
      t1_iter = mem_.find(t1);
      t2_iter = mem_.find(t2);
      if (t1_iter != mem_.end() && t2_iter != mem_.end()) {
	values.pop_back();
	mem_[cur] = min(costs.square + costs.compose + t1_iter->second,
			costs.compose + t2_iter->second);
      } else {
	if (t1_iter == mem_.end()) values.push_back(t1);
	if (t2_iter == mem_.end()) values.push_back(t2);
      }
      break;
    }
  }
  return mem_[x];
}

