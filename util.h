#pragma once
#ifndef UTIL_H_
#define UTIL_H_

#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>

#include <gmp.h>
#include <stdint.h>

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

#endif

