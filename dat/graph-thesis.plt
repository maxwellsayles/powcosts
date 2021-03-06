#!/usr/bin/gnuplot -persist

set terminal eps enhanced

set xlabel "The value of t for the exponent E_t"
set ylabel "Milliseconds"
set key left

# Binary vs NAF
set output 'binary_vs_naf-64.eps'
plot "binary-64.dat" with lines title 'Binary', \
     "naf_r2l-64.dat" with lines title 'Non-Adjacent Form'
set output 'binary_vs_naf-128.eps'
plot "binary-128.dat" with lines title 'Binary', \
     "naf_r2l-128.dat" with lines title 'Non-Adjacent Form'

# DBNS R2L
set output 'dbns_r2ls-64.eps'
plot "dbns_r2l-64.dat" with lines title "Non-Windowed R2L Chain", \
     "dbns_r2l36-64.dat" with lines title "2^23^2 Windowed R2L Chain"
set xrange [500:*]
set output 'dbns_r2ls-64-zoom.eps'
plot "dbns_r2l-64.dat" with lines title "Non-Windowed R2L Chain", \
     "dbns_r2l36-64.dat" with lines title "2^23^2 Windowed R2L Chain"
set xrange [*:*]
set output 'dbns_r2ls-128.eps'
plot "dbns_r2l-128.dat" with lines title "Non-Windowed R2L Chain", \
     "dbns_r2l36-128.dat" with lines title "2^23^2 Windowed R2L Chain"
set xrange [500:*]
set output 'dbns_r2ls-128-zoom.eps'
plot "dbns_r2l-128.dat" with lines title "Non-Windowed R2L Chain", \
     "dbns_r2l36-128.dat" with lines title "2^23^2 Windowed R2L Chain"
set xrange [*:*]

# DBNS L2R
set output 'dbns_l2rs-64.eps'
plot "dbns_l2r-64.dat" with lines title "L2R Representation", \
     "dbns_l2r_tree-64.dat" with lines title "L2R Best Approximations"
set xrange [500:*]
set output 'dbns_l2rs-64-zoom.eps'
plot "dbns_l2r-64.dat" with lines title "L2R Representation", \
     "dbns_l2r_tree-64.dat" with lines title "L2R Best Approximations"
set xrange [*:*]
set output 'dbns_l2rs-128.eps'
plot "dbns_l2r-128.dat" with lines title "L2R Representation", \
     "dbns_l2r_tree-128.dat" with lines title "L2R Best Approximations"
set xrange [500:*]
set output 'dbns_l2rs-128-zoom.eps'
plot "dbns_l2r-128.dat" with lines title "L2R Representation", \
     "dbns_l2r_tree-128.dat" with lines title "L2R Best Approximations"
set xrange [*:*]

# +/- variants
set output 'pm_variants-64.eps'
plot "pm1-64.dat" with lines title "Pruned Tree (x±1)/(2^c3^d)", \
     "pm2a3b-64.dat" with lines title "Pruned Tree (x±2^a3^b)/(2^c3^d)", \
     "pm2apm3b-64.dat" with lines title "Pruned Tree (x±2^a±3^b)/(2^c3^d)"
set xrange [500:*]
set output 'pm_variants-64-zoom.eps'
plot "pm1-64.dat" with lines title "Pruned Tree (x±1)/(2^c3^d)", \
     "pm2a3b-64.dat" with lines title "Pruned Tree (x±2^a3^b)/(2^c3^d)", \
     "pm2apm3b-64.dat" with lines title "Pruned Tree (x±2^a±3^b)/(2^c3^d)"
set xrange [*:*]
set output 'pm_variants-128.eps'
plot "pm1-128.dat" with lines title "Pruned Tree (x±1)/(2^c3^d)", \
     "pm2a3b-128.dat" with lines title "Pruned Tree (x±2^a3^b)/(2^c3^d)", \
     "pm2apm3b-128.dat" with lines title "Pruned Tree (x±2^a±3^b)/(2^c3^d)"
set xrange [500:*]
set output 'pm_variants-128-zoom.eps'
plot "pm1-128.dat" with lines title "Pruned Tree (x±1)/(2^c3^d)", \
     "pm2a3b-128.dat" with lines title "Pruned Tree (x±2^a3^b)/(2^c3^d)", \
     "pm2apm3b-128.dat" with lines title "Pruned Tree (x±2^a±3^b)/(2^c3^d)"
set xrange [*:*]

# block vs list
set output 'block_vs_list-64.eps'
plot "block-64.dat" with lines title "16-bit Blocks", \
     "list-64.dat" with lines title "Prime Factorization"
set output 'block_vs_list-128.eps'
plot "block-128.dat" with lines title "16-bit Blocks", \
     "list-128.dat" with lines title "Prime Factorization"

# winners
set output 'winners-64.eps'
plot "list-64.dat" with lines title "Prime Factorization", \
     "naf_r2l-64.dat" with lines title "Non-Adjacent Form", \
     "dbns_r2l36-64.dat" with lines title "2^23^3 Windowed R2L Chain", \
     "dbns_l2r_tree-64.dat" with lines title "L2R Best Approximations", \
     "pm2a3b-64.dat" with lines title "Pruned Tree (x±2^a3^b)/(2^c3^d)"
set output 'winners-128.eps'
plot "list-128.dat" with lines title "Prime Factorization", \
     "naf_r2l-128.dat" with lines title "Non-Adjacent Form", \
     "dbns_r2l36-128.dat" with lines title "2^23^3 Windowed R2L Chain", \
     "dbns_l2r_tree-128.dat" with lines title "L2R Best Approximations", \
     "pm2a3b-128.dat" with lines title "Pruned Tree (x±2^a3^b)/(2^c3^d)"
