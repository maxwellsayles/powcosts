#ifndef TERM__INCLUDED
#define TERM__INCLUDED

#include <iostream>

#include <stdint.h>

struct term {
  friend std::ostream& operator<<(std::ostream& os, const term& t);
  
  term() : sign(1), a(0), b(0) {}
  term(int in_sign, int in_a, int in_b) : sign(in_sign), a(in_a), b(in_b) {}
  
  void neg() {
    sign = -sign;
  }

  int8_t sign;
  int16_t a;
  int16_t b;
};

struct term_order {
  bool operator()(const term& x, const term& y) const {
    return (x.a < y.a);
  }
};

inline std::ostream& operator<<(std::ostream& os, const term& t) {
  if (t.sign == -1) {
    os << "-";
  } else {
    os << "+";
  }
  os << "2^" << t.a << "*3^" << t.b;
  return os;
}

typedef struct {
  int8_t sign;
  int16_t a;
  int16_t b;
} term_t;

/**
 * Representation is given as an array 'A' of factored_term_t such that
 * N = ((((A[0].b*A[0].a) \pm A[1].b)*A[1].a) \pm A[2].b)*A[2].a ....
 *
 * where A[i].b is added if the high bit of b is clear,
 * and subtracted if the high bit of b is set.
 */
typedef struct {
  uint16_t a;
  uint16_t b;
} factored_term_t;

#endif

