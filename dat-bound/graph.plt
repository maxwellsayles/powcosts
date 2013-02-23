#!/usr/bin/gnuplot -persist

set terminal eps

# Graph how a DBNS l2r varies as the bounds vary
set output 'dbns_l2r_vary_max.eps'
set xlabel "Maximum squares permitted."
set ylabel "Milliseconds"
plot "dbns_l2r_vary_max-64.dat" with lines title '59-bit discriminant (64-bit implementation)', \
     "dbns_l2r_vary_max-128.dat" with lines title '118-bit discriminant (128-bit implementation)'

# Graph the pm2a3b as we vary the bound on a and b.
set xlabel "Maximum number of squares and cubes."
set ylabel "Milliseconds"
set output 'pm2a3b_vary_max-64.eps'
plot "pm2a3b_vary_max-64.dat" with lines title '59-bit discriminant (64-bit implementation)'
set output 'pm2a3b_vary_max-128.eps'
plot "pm2a3b_vary_max-128.dat" with lines title '118-bit discriminant (128-bit implementation)' lt 2

