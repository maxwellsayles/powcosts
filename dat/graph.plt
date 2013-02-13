#!/usr/bin/gnuplot -persist

set terminal eps

# Graph how a DBNS l2r varies as the bounds vary
set output 'dbns_l2r_vary_bounds.eps'
set xlabel "Maximum squares permitted."
set ylabel "Milliseconds"
plot "dbns_l2r_vary_bounds-64.dat" with lines title '64-bit', \
     "dbns_l2r_vary_bounds-128.dat" with lines title '128-bit'
