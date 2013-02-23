#pragma once
#ifndef UTIL_H_
#define UTIL_H_

#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>

#include <gmp.h>
#include <stdint.h>
#include <sys/time.h>
#include <time.h>

#include "powcosts/mpz_c.h"

/// Write a gnuplot data file
void write_gnuplot_datfile(const std::string& filename,
			   const uint64_t* x,
			   const double* y,
			   const int sample_count);

/// Append a row to a gnuplot data file.
void append_gnuplot_datfile(const std::string& filename,
			    const uint64_t x,
			    const double y);

/// \f$ v = 2^a * 3^b \f$
inline void two_three_val(mpz_t v, const int a, const int b) {
  mpz_ui_pow_ui(v, 3, b);
  mpz_mul_2exp(v, v, a);
}

/**
 * True if x is of the form \f$ s2^a3^b \f$ where s in {1,-1}.
 * a and b are output paramters.
 */
bool is_2_3_integer(const mpz_t x, int* a, int* b);

/**
 * Computes a 2^a3^b such that the distance from n is minimal.
 * Returns the value of the best approximation (2^a3^b).
 */
mpz_c best_db_approx(int* out_a,
		     int* out_b,
		     const mpz_t n,
		     const int max_a,
		     const int max_b);

/// Gives the time from system on in nanoseconds
static inline uint64_t current_nanos(void) {
#ifdef __linux__
  struct timespec res;
  clock_gettime(CLOCK_MONOTONIC_RAW, &res);
  return (res.tv_sec * 1000000000ULL) + res.tv_nsec;
#else
  struct timeval tv;
  gettimeofday(&tv, 0);
  return ((uint64_t)tv.tv_sec * 1000000ULL + (uint64_t)tv.tv_usec) * 1000;
#endif
}


#endif

