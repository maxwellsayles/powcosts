/**
 * @file mpz_c.h
 * Small C++ wrapper for mpz_t.
 * 
 * Previous versions of this file used low-level functions
 * to perform division by 2 and 3, but were later commented out and
 * replaced with higher level versions, presumably because there
 * was some error when using the low-level version.
 */
#pragma once
#ifndef MPZ_C__INCLUDED
#define MPZ_C__INCLUDED

#include <gmp.h>
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#include <stdint.h>

extern "C" {
#include "liboptarith/math_mpz.h"
}

class mpz_c {
 public:
  mpz_c() {
    mpz_init(z);
  }

  mpz_c(const char* x) {
    mpz_init_set_str(z, x, 10);
  }

  mpz_c(const uint32_t x) {
    mpz_init_set_ui(z, x);
  }
  
  mpz_c(const mpz_t that) {
    mpz_init_set(z, that);
  }
  
  mpz_c(const mpz_c& that) {
    mpz_init_set(z, that.z);
  }
  
  virtual ~mpz_c() {
    mpz_clear(z);
  }

  mpz_c& operator=(const char* x) {
    mpz_set_str(z, x, 10);
    return *this;
  }
  
  mpz_c& operator=(const uint32_t x) {
    mpz_set_ui(z, x);
    return *this;
  }

  mpz_c& operator=(const mpz_t that) {
    mpz_set(z, that);
    return *this;
  }

  mpz_c& operator=(const mpz_c& that) {
    mpz_set(z, that.z);
    return *this;
  }

  bool operator<(const mpz_c& that) const {
    return (mpz_cmp(z, that.z) < 0);
  }

  bool operator>(const mpz_c& that) const {
    return (mpz_cmp(z, that.z) > 0);
  }

  bool operator==(const mpz_c& that) const {
    return (mpz_cmp(z, that.z) == 0);
  }

  bool operator!=(const mpz_c& that) const {
    return (mpz_cmp(z, that.z) != 0);
  }
  
  bool operator>=(const mpz_c& that) const {
    return (mpz_cmp(z, that.z) >= 0);
  }

  bool operator<=(const mpz_c& that) const {
    return (mpz_cmp(z, that.z) <= 0);
  }

  int mod3() const {
    return mpz_mod3(z);
  }

  int mod9() const {
    return mpz_mod9(z);
  }

  void div2(const mpz_c& that) {
    mpz_tdiv_q_2exp(z, that.z, 1);
  }

  void div2() {
    mpz_tdiv_q_2exp(z, z, 1);
  }

  void div3(const mpz_c& that) {
    mpz_divexact_ui(z, that.z, 3);
  }

  void div3() {
    mpz_divexact_ui(z, z, 3);
  }
  
  /// this = this / 2^a where a <= max2.
  /// Return a.
  int reduce2(int32_t max2 = INT32_MAX) {
    int res = mpz_scan1(z, 0);
    if (res > max2) res = max2;
    if (res > 0) {
      mpz_tdiv_q_2exp(z, z, res);
    }
    return res;
  }
  
  /// this = this / 3^b where b <= max3.
  /// Return b.
  int reduce3(int32_t max3 = INT32_MAX) {
    int res = 0;
    while (mpz_mod3(z) == 0 && res < max3) {
      div3();
      res ++;
    }
    return res;
  }
  
  mpz_t z;
};

#endif


