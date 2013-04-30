# Graphing these 16-bit timings are very noisy, and only
# graphing every 256-or-so biases the sample set.

set terminal eps
set key left
set xlabel 'Exponent n'
set ylabel 'Nanoseconds'

set output 'add_memo_chain-64.eps'
plot 'add_chain_65536-64.dat' every 256 title "s'(n)", \
     'memo_chain_65536-64.dat' every 256 title "f(n)"

set output 'add_memo_chain-128.eps'
plot 'add_chain_65536-128.dat' every 256 title "s'(n)", \
     'memo_chain_65536-128.dat' every 256 title "f(n)"
     