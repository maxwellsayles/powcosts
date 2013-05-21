set terminal eps enhanced
set key left

set xlabel 'The value of t for the exponent E_t'
set ylabel 'Estimated Time (Milliseconds)

set xrange [0:1000]

set output 'pow-winners-59.eps'
set yrange [0:8]
plot 'binary-64.dat' with lines t 'Binary', \
     'naf_r2l-64.dat' with lines t 'Non-Adjacent Form', \
     'pm2a3b-64.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r-64.dat' with lines t 'Greedy L2R', \
     'dbns_l2r_tree-64.dat' with lines t 'Our L2R Best Approximations'

set output 'pow-winners-118.eps'
set yrange [0:20]
plot 'binary-128.dat' with lines t 'Binary', \
     'naf_r2l-128.dat' with lines t 'Non-Adjacent Form', \
     'pm2a3b-128.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r-128.dat' with lines t 'Greedy L2R', \
     'dbns_l2r_tree-128.dat' with lines t 'Our L2R Best Approximations'
