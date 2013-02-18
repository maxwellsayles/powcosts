#!/usr/bin/gnuplot -persist

set terminal eps

# Graph how a DBNS l2r varies as the bounds vary
set output 'dbns_l2r_vary_bounds.eps'
set xlabel "Maximum squares permitted."
set ylabel "Milliseconds"
plot "dbns_l2r_vary_bounds-64.dat" with lines title '59-bit discriminant (64-bit implementation)', \
     "dbns_l2r_vary_bounds-128.dat" with lines title '118-bit discriminant (128-bit implementation)'

# Graph the difference between additive chains +/- chains.
set output 'memo_chains-64.eps'
set xlabel "Value of Exponent"
set ylabel "Nanoseconds"
plot "add_chain_sub_memo_chain-64.dat" every 512 title "Additive Chain (64-bit)", \
     "memo_chain_sub_add_chain-64.dat" every 512 title "+/- Chain (64-bit)"
set output 'memo_chains-128.eps'
set xlabel "Value of Exponent"
set ylabel "Nanoseconds"
plot "add_chain_sub_memo_chain-128.dat" every 512 title "Additive Chain (128-bit)", \
     "memo_chain_sub_add_chain-128.dat" every 512 title "+/- Chain (128-bit)"

# Graph the difference between +/- chains and incremental search.
set output 'pre_vs_memo-64.eps'
set xlabel "Value of Exponent"
set ylabel "Nanoseconds"
plot "memo_chain_sub_pre-64.dat" every 512 title "+/- Chain (64-bit)", \
     "pre_sub_memo_chain-64.dat" every 512 title "Incremental Search (64-bit)"
set output 'pre_vs_memo-128.eps'
set xlabel "Value of Exponent"
set ylabel "Nanoseconds"
plot "memo_chain_sub_pre-128.dat" every 512 title "+/- Chain (128-bit)", \
     "pre_sub_memo_chain-128.dat" every 512 title "Incremental Search (128-bit)"

