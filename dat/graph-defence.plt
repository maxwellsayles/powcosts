set terminal eps enhanced
set key left

set xlabel 'The value of t for the exponent E_t'
set ylabel 'Average Time (Milliseconds)

set xrange [0:1000]

set output 'pow-winners-64.eps'
plot 'naf_r2l-64.dat' with lines t 'Non-Adjacent Form', \
     'dbns_r2l36-64.dat' with lines t '2^23^2 Windowed R2L Chain', \
     'list-64.dat' with lines t 'Prime Factorization', \
     'pm2a3b-64.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r_tree-64.dat' with lines t 'L2R Best Approximations'

set output 'pow-winners-128.eps'
plot 'naf_r2l-128.dat' with lines t 'Non-Adjacent Form', \
     'dbns_r2l36-128.dat' with lines t '2^23^2 Windowed R2L Chain', \
     'list-128.dat' with lines t 'Prime Factorization', \
     'pm2a3b-128.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r_tree-128.dat' with lines t 'L2R Best Approximations'
