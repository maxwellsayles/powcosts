#!/usr/bin/gnuplot -persist

set terminal eps

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

