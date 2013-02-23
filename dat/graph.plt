#!/usr/bin/gnuplot -persist

set terminal eps enhanced

set xlabel "Primorial"
set ylabel "Milliseconds"
set key left

# Binary vs NAF
set output 'binary_vs_naf-64.eps'
set title 'Base 2 (59-bit Discriminants)'
plot "binary-64.dat" with lines title 'Binary', \
     "naf_r2l-64.dat" with lines title 'R2L NAF'
set output 'binary_vs_naf-128.eps'
set title 'Base 2 (118-bit Discriminants)'
plot "binary-128.dat" with lines title 'Binary', \
     "naf_r2l-128.dat" with lines title 'R2L NAF'

# DBNS R2L
set title 'Right-to-Left 2,3 Chains (59-bit Discriminants)'
set output 'dbns_r2ls-64.eps'
plot "dbns_r2l-64.dat" with lines title "Non-Windowed", \
     "dbns_r2l36-64.dat" with lines title "2^23^2 Windowed"
set xrange [2000:*]
set output 'dbns_r2ls-64-zoom.eps'
set title 'Right-to-Left 2,3 Chains (59-bit Discriminants - Zoomed)'
plot "dbns_r2l-64.dat" with lines title "Non-Windowed", \
     "dbns_r2l36-64.dat" with lines title "2^23^2 Windowed"
set xrange [*:*]
set output 'dbns_r2ls-128.eps'
set title 'Right-to-Left 2,3 Chains (118-bit Discriminants)'
plot "dbns_r2l-128.dat" with lines title "Non-Windowed", \
     "dbns_r2l36-128.dat" with lines title "2^23^2 Windowed"
set xrange [2000:*]
set output 'dbns_r2ls-128-zoom.eps'
set title 'Right-to-Left 2,3 Chains (118-bit Discriminants - Zoomed)'
plot "dbns_r2l-128.dat" with lines title "Non-Windowed", \
     "dbns_r2l36-128.dat" with lines title "2^23^2 Windowed"
set xrange [*:*]

# DBNS L2R
set output 'dbns_l2rs-64.eps'
set title 'Left-to-Right 2,3 Representations (59-bit Discriminants)'
plot "dbns_l2r-64.dat" with lines title "L2R 2,3 Reps", \
     "dbns_l2r_tree-64.dat" with lines title "4 Best ||x|-2^a3^b|"
set xrange [2000:*]
set output 'dbns_l2rs-64-zoom.eps'
set title 'Left-to-Right 2,3 Representations (59-bit Discriminants - Zoomed)'
plot "dbns_l2r-64.dat" with lines title "L2R 2,3 Reps", \
     "dbns_l2r_tree-64.dat" with lines title "4 Best ||x|-2^a3^b|"
set xrange [*:*]
set output 'dbns_l2rs-128.eps'
set title 'Left-to-Right 2,3 Representations (118-bit Discriminants)'
plot "dbns_l2r-128.dat" with lines title "L2R 2,3 Reps", \
     "dbns_l2r_tree-128.dat" with lines title "4 Best ||x|-2^a3^b|"
set xrange [2000:*]
set output 'dbns_l2rs-128-zoom.eps'
set title 'Left-to-Right 2,3 Representations (118-bit Discriminants - Zoomed)'
plot "dbns_l2r-128.dat" with lines title "L2R 2,3 Reps", \
     "dbns_l2r_tree-128.dat" with lines title "4 Best ||x|-2^a3^b|"
set xrange [*:*]

# +/- variants
set output 'pm_variants-64.eps'
set title "Tree Based x'/2^c3^d (59-bit Discriminants)"
plot "pm1-64.dat" with lines title "x±1", \
     "pm2a3b-64.dat" with lines title "x±2^a3^b", \
     "pm2apm3b-64.dat" with lines title "x±2^a±3^b"
set xrange [2000:*]
set output 'pm_variants-64-zoom.eps'
set title "Tree Based x'/2^c3^d (59-bit Discriminants - Zoomed)"
plot "pm1-64.dat" with lines title "x±1", \
     "pm2a3b-64.dat" with lines title "x±2^a3^b", \
     "pm2apm3b-64.dat" with lines title "x±2^a±3^b"
set xrange [*:*]
set output 'pm_variants-128.eps'
set title "Tree Based x'/2^c3^d (118-bit Discriminants)"
plot "pm1-128.dat" with lines title "x±1", \
     "pm2a3b-128.dat" with lines title "x±2^a3^b", \
     "pm2apm3b-128.dat" with lines title "x±2^a±3^b"
set xrange [2000:*]
set output 'pm_variants-128-zoom.eps'
set title "Tree Based x'/2^c3^d (118-bit Discriminants - Zoomed)"
plot "pm1-128.dat" with lines title "x±1", \
     "pm2a3b-128.dat" with lines title "x±2^a3^b", \
     "pm2apm3b-128.dat" with lines title "x±2^a±3^b"
set xrange [*:*]

# block vs list
set output 'block_vs_list-64.eps'
set title 'Use g^b for 16-bit b (59-bit Discriminants)'
plot "block-64.dat" with lines title "16-bit Blocks", \
     "list-64.dat" with lines title "Prime Factorization"
set output 'block_vs_list-128.eps'
set title 'Use g^b for 16-bit b (118-bit Discriminants)'
plot "block-128.dat" with lines title "16-bit Blocks", \
     "list-128.dat" with lines title "Prime Factorization"

# winners
set output 'winners-64.eps'
set title 'Best Performers (59-bit Discriminants)'
plot "list-64.dat" with lines title "Prime Factorization", \
     "naf_r2l-64.dat" with lines title "R2L NAF", \
     "dbns_r2l36-64.dat" with lines title "2^23^3 Windowed R2L 2,3 Chains", \
     "dbns_l2r_tree-64.dat" with lines title "4 Best ||x|-2^a3^b|", \
     "pm2a3b-64.dat" with lines title "4 Best (x±2^a3^b)/(2^c3^d)"
set output 'winners-128.eps'
set title 'Best Performers (118-bit Discriminants)'
plot "list-128.dat" with lines title "Prime Factorization", \
     "naf_r2l-128.dat" with lines title "R2L NAF", \
     "dbns_r2l36-128.dat" with lines title "2^23^3 Windowed R2L 2,3 Chains", \
     "dbns_l2r_tree-128.dat" with lines title "4 Best ||x|-2^a3^b|", \
     "pm2a3b-128.dat" with lines title "4 Best (x±2^a3^b)/(2^c3^d)"
