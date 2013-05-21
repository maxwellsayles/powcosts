set terminal eps enhanced

set xlabel 'The value of t for the exponent E_t'
set ylabel 'Estimated Time (Milliseconds)'

set xrange [0:1000]

set output 'pow-winners-16.eps'
plot 'binary-16.dat' with lines t 'Binary', \
     'naf_r2l-16.dat' with lines t 'Non-Adjacent Form', \
     'pm2a3b-16.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r-16.dat' with lines t 'Greedy L2R', \
     'dbns_l2r_tree-16.dat' with lines t 'Our L2R Best Approximations'

set output 'pow-winners-32.eps'
plot 'binary-32.dat' with lines t 'Binary', \
     'naf_r2l-32.dat' with lines t 'Non-Adjacent Form', \
     'pm2a3b-32.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r-32.dat' with lines t 'Greedy L2R', \
     'dbns_l2r_tree-32.dat' with lines t 'Our L2R Best Approximations'

set output 'pow-winners-48.eps'
plot 'binary-48.dat' with lines t 'Binary', \
     'naf_r2l-48.dat' with lines t 'Non-Adjacent Form', \
     'pm2a3b-48.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r-48.dat' with lines t 'Greedy L2R', \
     'dbns_l2r_tree-48.dat' with lines t 'Our L2R Best Approximations'

set output 'pow-winners-64.eps'
plot 'binary-64.dat' with lines t 'Binary', \
     'naf_r2l-64.dat' with lines t 'Non-Adjacent Form', \
     'pm2a3b-64.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r-64.dat' with lines t 'Greedy L2R', \
     'dbns_l2r_tree-64.dat' with lines t 'Our L2R Best Approximations'

set output 'pow-winners-80.eps'
plot 'binary-80.dat' with lines t 'Binary', \
     'naf_r2l-80.dat' with lines t 'Non-Adjacent Form', \
     'pm2a3b-80.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r-80.dat' with lines t 'Greedy L2R', \
     'dbns_l2r_tree-80.dat' with lines t 'Our L2R Best Approximations'

set output 'pow-winners-96.eps'
plot 'binary-96.dat' with lines t 'Binary', \
     'naf_r2l-96.dat' with lines t 'Non-Adjacent Form', \
     'pm2a3b-96.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r-96.dat' with lines t 'Greedy L2R', \
     'dbns_l2r_tree-96.dat' with lines t 'Our L2R Best Approximations'

set output 'pow-winners-112.eps'
plot 'binary-112.dat' with lines t 'Binary', \
     'naf_r2l-112.dat' with lines t 'Non-Adjacent Form', \
     'pm2a3b-112.dat' with lines t 'Pruned Tree (x±2^a3^b)/(2^c3^d)', \
     'dbns_l2r-112.dat' with lines t 'Greedy L2R', \
     'dbns_l2r_tree-112.dat' with lines t 'Our L2R Best Approximations'

     