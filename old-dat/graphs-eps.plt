#!/usr/bin/gnuplot -persist


set terminal postscript eps color "Arial"
set output 'primorial-s64.eps'
#set title "Timings for s64 primorial" 
set xlabel "Bound B for Primorial" 
set ylabel "Relative timing" 
set xrange[0:2000]
plot "naf_r2l_s64.dat" with lines title 'naf', \
	"dbns_chain_r2l36_s64.dat" with lines title 'r2l', \
	"dbns_l2r_s64.dat" with lines title 'l2r', \
	"prune_dbns_l2r_s64.dat" with lines title 'pruned l2r', \
	"dbns_pre_block_s64.dat" with lines title 'search'
