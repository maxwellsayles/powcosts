#include "powcosts/util.h"

using namespace std;

void write_gnuplot_datfile(const string& filename,
			   const uint64_t* x,
			   const double* y,
			   const int sample_count) {
  ofstream f(filename.c_str());
  f << setprecision(5) << fixed;
  for (int i = 0;  i < sample_count;  i ++) {
    f << x[i] << ", " << y[i] << endl;
  }
  f.close();
}

void append_gnuplot_datfile(const string& filename,
			    const uint64_t x,
			    const double y) {
  ofstream f;
  f.open(filename.c_str(), fstream::out | fstream::app);
  f << setprecision(5) << fixed;
  f << x << ", " << y << endl;
  f.close();
}

bool is_2_3_integer(const mpz_t x, int* a, int* b) {
  mpz_c t;

  // make sure x is not zero
  if (mpz_cmp_ui(x, 0) == 0) {
    (*a) = 0;
    (*b) = 0;
    return false;
  }

  // remove powers of 2
  (*a) = mpz_scan1(x, 0);
  if (*a > 0) {
    mpz_tdiv_q_2exp(t.z, x, *a);
  } else {
    mpz_set(t.z, x);
  }

  // remove powers of 3
  (*b) = 0;
  while (mpz_mod3(t.z) == 0) {
    mpz_divexact_ui(t.z, t.z, 3);
    (*b)++;
  }

  return (mpz_cmpabs_ui(t.z, 1) == 0);
}

mpz_c best_db_approx(int* out_a,
		     int* out_b,
		     const mpz_t n,
		     const int max_a,
		     const int max_b) {
  if (is_2_3_integer(n, out_a, out_b)) {
    return n;
  }

  mpz_c approx;
  mpz_c best_d;
  mpz_c d;
  mpz_c v;
  mpz_c tmp_v;
  int best_a = max_a;
  int best_b = max_b;
  int a;
  int b;
  int tmp;

  mpz_set(best_d.z, n);

  // find largest 'a' such that 2^a <= n
  a = mpz_sizeinbase(n, 2) - 1;
  if (a > max_a) a = max_a;
  mpz_set_ui(v.z, 1);
  mpz_mul_2exp(v.z, v.z, a);

  // find largest 'b' such that 2^a3^b <= n
  b = 0;
  mpz_set(tmp_v.z, v.z);
  while (mpz_cmp(tmp_v.z, n) <= 0) {
    mpz_set(v.z, tmp_v.z);

    // tmp_v = v*3
    mpz_mul_2exp(tmp_v.z, v.z, 1);
    mpz_add(tmp_v.z, tmp_v.z, v.z);
        
    b ++;
  }
  b --;

  // iterate all values of 'a'
  while (a >= 0 && b <= max_b) {
    // check 2^a*3^b
    mpz_sub(d.z, n, v.z);
    tmp = mpz_cmpabs(d.z, best_d.z);
    if (tmp < 0) {
      mpz_set(approx.z, v.z);
      mpz_set(best_d.z, d.z);
      best_a = a;
      best_b = b;
    }

    // check 2^{a+1}*3^b
    if (a+1 <= max_a) {
      mpz_sub(d.z, d.z, v.z);
      tmp = mpz_cmpabs(d.z, best_d.z);
      if (tmp < 0) {
	mpz_mul_2exp(approx.z, v.z, 1);
	mpz_set(best_d.z, d.z);
	best_a = a+1;
	best_b = b;
      }
    }

    // next 'a'
    a --;
    mpz_tdiv_q_2exp(v.z, v.z, 1);

    // find largest 'b' such that 2^a3^b <= n
    mpz_set(tmp_v.z, v.z);
    while (mpz_cmp(tmp_v.z, n) <= 0) {
      mpz_set(v.z, tmp_v.z);

      // tmp_v = v*3
      mpz_mul_2exp(tmp_v.z, v.z, 1);
      mpz_add(tmp_v.z, tmp_v.z, v.z);
      b ++;
    }
    b --;
  }

  *out_a = best_a;
  *out_b = best_b;
  return approx;
}
