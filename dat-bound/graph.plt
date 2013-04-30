#!/usr/bin/gnuplot -persist

set terminal eps enhanced

# Graph how a DBNS l2r varies as the bounds vary
set xrange[0:11271]
set output 'dbns_l2r_vary_max.eps'
set xlabel "Maximum squares permitted."
set ylabel "Milliseconds"
plot "dbns_l2r_vary_max-64.dat" with lines title '59-bit discriminants', \
     "dbns_l2r_vary_max-128.dat" with lines title '118-bit discriminants'

# Graph the pm2a3b as we vary the bound on a and b.
set xlabel "Maximum number of squares and cubes."
set ylabel "Milliseconds"
set xrange [1:32]
set output 'pm2a3b_vary_max-64.eps'
plot "pm2a3b_vary_max-64.dat" with lines title 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     "pm2apm3b_vary_max-64.dat" with lines title 'Pruned Tree (x±2^a±3^b)/(2^c3^d)'
set output 'pm2a3b_vary_max-128.eps'
plot "pm2a3b_vary_max-128.dat" with lines title 'Pruned Tree (x±2^a3^b)/(2^c3^d)' lt 3, \
     "pm2apm3b_vary_max-128.dat" with lines title 'Pruned Tree (x±2^a±3^b)/(2^c3^d)' lt 4
