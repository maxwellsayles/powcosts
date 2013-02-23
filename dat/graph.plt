#!/usr/bin/gnuplot -persist

set terminal eps enhanced

set xlabel "Primorial"
set ylabel "Milliseconds"

# Binary vs NAF
set output 'binary_vs_naf-64.eps'
plot "binary-64.dat" with lines title 'Binary (59-bit)', \
     "naf_r2l-64.dat" with lines title 'NAF-R2L (59-bit)'
set output 'binary_vs_naf-128.eps'
plot "binary-128.dat" with lines title 'Binary (118-bit)', \
     "naf_r2l-128.dat" with lines title 'NAF-R2L (118-bit)'

# DBNS R2L
set output 'dbns_r2ls-64.eps'
plot "dbns_r2l-64.dat" with lines title "DBNS R2L (59-bit)", \
     "dbns_r2l36-64.dat" with lines title "DBNS R2L (59-bit)"
set xrange [2000:*]
set output 'dbns_r2ls-64-zoom.eps'
plot "dbns_r2l-64.dat" with lines title "DBNS R2L (59-bit)", \
     "dbns_r2l36-64.dat" with lines title "DBNS R2L (59-bit)"
set xrange [*:*]
set output 'dbns_r2ls-128.eps'
plot "dbns_r2l-128.dat" with lines title "DBNS R2L (118-bit)", \
     "dbns_r2l36-128.dat" with lines title "DBNS R2L (118-bit)"
set xrange [2000:*]
set output 'dbns_r2ls-128-zoom.eps'
plot "dbns_r2l-128.dat" with lines title "DBNS R2L (118-bit)", \
     "dbns_r2l36-128.dat" with lines title "DBNS R2L (118-bit)"
set xrange [*:*]

# DBNS L2R
set output 'dbns_l2rs-64.eps'
plot "dbns_l2r-64.dat" with lines title "DBNS L2R (59-bit)", \
     "dbns_l2r_tree-64.dat" with lines title "16 Best DBNS L2R (59-bit)"
set xrange [2000:*]
set output 'dbns_l2rs-64-zoom.eps'
plot "dbns_l2r-64.dat" with lines title "DBNS L2R (59-bit)", \
     "dbns_l2r_tree-64.dat" with lines title "16 Best DBNS L2R (59-bit)"
set xrange [*:*]
set output 'dbns_l2rs-128.eps'
plot "dbns_l2r-128.dat" with lines title "DBNS L2R (118-bit)", \
     "dbns_l2r_tree-128.dat" with lines title "16 Best DBNS L2R (118-bit)"
set xrange [2000:*]
set output 'dbns_l2rs-128-zoom.eps'
plot "dbns_l2r-128.dat" with lines title "DBNS L2R (118-bit)", \
     "dbns_l2r_tree-128.dat" with lines title "16 Best DBNS L2R (118-bit)"
set xrange [*:*]

# +/- variants
set output 'pm_variants-64.eps'
plot "pm1-64.dat" with lines title "± 1 (59-bit)", \
     "pm2a3b-64.dat" with lines title "± 2^a3^b (59-bit)", \
     "pm2apm3b-64.dat" with lines title "± 2^a ± 3^b (59-bit)"
set xrange [2000:*]
set output 'pm_variants-64-zoom.eps'
plot "pm1-64.dat" with lines title "± 1 (59-bit)", \
     "pm2a3b-64.dat" with lines title "± 2^a3^b (59-bit)", \
     "pm2apm3b-64.dat" with lines title "± 2^a ± 3^b (59-bit)"
set xrange [*:*]
set output 'pm_variants-128.eps'
plot "pm1-128.dat" with lines title "± 1 (118-bit)", \
     "pm2a3b-128.dat" with lines title "± 2^a3^b (118-bit)", \
     "pm2apm3b-128.dat" with lines title "± 2^a ± 3^b (118-bit)"
set xrange [2000:*]
set output 'pm_variants-128-zoom.eps'
plot "pm1-128.dat" with lines title "± 1 (118-bit)", \
     "pm2a3b-128.dat" with lines title "± 2^a3^b (118-bit)", \
     "pm2apm3b-128.dat" with lines title "± 2^a ± 3^b (118-bit)"
set xrange [*:*]

# block vs list
set output 'block_vs_list-64.eps'
plot "block-64.dat" with lines title "16-bit Blocks (59-bit)", \
     "list-64.dat" with lines title "Prime Factorization (59-bit)"
set output 'block_vs_list-128.eps'
plot "block-128.dat" with lines title "16-bit Blocks (118-bit)", \
     "list-128.dat" with lines title "Prime Factorization (118-bit)"     