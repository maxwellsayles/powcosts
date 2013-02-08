#!/usr/bin/gnuplot -persist


#############
# primorial #
#############

set terminal png 
set output 'primorial-new.png'
set title "Timings for s64 primorial" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set xrange[0:2000]
plot "naf_r2l_s64.dat" with lines title 'naf-r2l', \
	"dbns_l2r_s64.dat" with lines title 'dbns-l2r', \
	"subreduce_s64.dat" with lines title 'pm-reduce', \
	"prune_dbns_l2r_s64.dat" with lines title 'prune-closest', \
	"prune_subreduce_s64.dat" with lines title 'prune-pm-reduce'




set terminal png 
set output 'primorial-s64.png'
set title "Timings for s64 primorial" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "naf_r2l_s64.dat" with lines title 'naf-r2l', \
	"dbns_l2r_s64.dat" with lines title 'dbns-l2r', \
	"dbns_chain_r2l_s64.dat" with lines title 'dbns-chain-r2l', \
	"dbns_chain_r2l36_s64.dat" with lines title 'dbns-chain-r2l36', \
	"dbns_pre_block_s64.dat" with lines title 'dbns-pre'

set terminal png 
set output 'primorial-s128-lo.png'
set title "Timings for s128-lo primorial" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "naf_r2l_s128_lo.dat" with lines title 'naf-r2l', \
	"dbns_l2r_s128_lo.dat" with lines title 'dbns-l2r', \
	"dbns_chain_r2l_s128_lo.dat" with lines title 'dbns-chain-r2l', \
	"dbns_chain_r2l36_s128_lo.dat" with lines title 'dbns-chain-r2l36', \
	"dbns_pre_block_s128_lo.dat" with lines title 'dbns-pre'

set terminal png 
set output 'primorial-s128-hi.png'
set title "Timings for s128-hi primorial" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "naf_r2l_s128_hi.dat" with lines title 'naf-r2l', \
	"dbns_l2r_s128_hi.dat" with lines title 'dbns-l2r', \
	"dbns_chain_r2l_s128_hi.dat" with lines title 'dbns-chain-r2l', \
	"dbns_chain_r2l36_s128_hi.dat" with lines title 'dbns-chain-r2l36', \
	"dbns_pre_block_s128_hi.dat" with lines title 'dbns-pre'

set terminal png 
set output 'primorial-mpz.png'
set title "Timings for mpz primorial" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "naf_r2l_mpz.dat" with lines title 'naf-r2l', \
	"dbns_l2r_mpz.dat" with lines title 'dbns-l2r', \
	"dbns_chain_r2l_mpz.dat" with lines title 'dbns-chain-r2l', \
	"dbns_chain_r2l36_mpz.dat" with lines title 'dbns-chain-r2l36', \
	"dbns_pre_block_mpz.dat" with lines title 'dbns-pre'


################
# prime powers #
################

set terminal png 
set output 'prime-powers-s64.png'
set title "Timings for s64 prime powers" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "naf_r2l_list_s64.dat" with lines title 'naf-r2l', \
	"dbns_l2r_list_s64.dat" with lines title 'dbns-l2r', \
	"dbns_chain_r2l_list_s64.dat" with lines title 'dbns-chain-r2l', \
	"dbns_chain_r2l36_list_s64.dat" with lines title 'dbns-chain-r2l36', \
	"dbns_pre_list_s64.dat" with lines title 'dbns-pre'

set terminal png 
set output 'prime-powers-s128-lo.png'
set title "Timings for s128-lo prime powers" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "naf_r2l_list_s128_lo.dat" with lines title 'naf-r2l', \
	"dbns_l2r_list_s128_lo.dat" with lines title 'dbns-l2r', \
	"dbns_chain_r2l_list_s128_lo.dat" with lines title 'dbns-chain-r2l', \
	"dbns_chain_r2l36_list_s128_lo.dat" with lines title 'dbns-chain-r2l36', \
	"dbns_pre_list_s128_lo.dat" with lines title 'dbns-pre'

set terminal png 
set output 'prime-powers-s128-hi.png'
set title "Timings for s128-hi prime powers" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "naf_r2l_list_s128_hi.dat" with lines title 'naf-r2l', \
	"dbns_l2r_list_s128_hi.dat" with lines title 'dbns-l2r', \
	"dbns_chain_r2l_list_s128_hi.dat" with lines title 'dbns-chain-r2l', \
	"dbns_chain_r2l36_list_s128_hi.dat" with lines title 'dbns-chain-r2l36', \
	"dbns_pre_list_s128_hi.dat" with lines title 'dbns-pre'

set terminal png 
set output 'prime-powers-mpz.png'
set title "Timings for mpz prime powers" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "naf_r2l_list_mpz.dat" with lines title 'naf-r2l', \
	"dbns_l2r_list_mpz.dat" with lines title 'dbns-l2r', \
	"dbns_chain_r2l_list_mpz.dat" with lines title 'dbns-chain-r2l', \
	"dbns_chain_r2l36_list_mpz.dat" with lines title 'dbns-chain-r2l36', \
	"dbns_pre_list_mpz.dat" with lines title 'dbns-pre'


################
# Winners #
################

set terminal png 
set output 'winners-s64.png'
set title "Best s64 primorial vs prime powers" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "dbns_l2r_s64.dat" with lines title 'dbns-l2r primorial', \
	"dbns_pre_list_s64.dat" with lines title 'dbns-pre prime power'

set terminal png 
set output 'winners-s128-lo.png'
set title "Best s128-lo primorial vs prime powers" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "dbns_l2r_s128_lo.dat" with lines title 'dbns-l2r primorial', \
	"dbns_pre_list_s128_lo.dat" with lines title 'dbns-pre prime power'

set terminal png 
set output 'winners-s128-hi.png'
set title "Best s128-hi primorial vs prime powers" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "dbns_l2r_s128_hi.dat" with lines title 'dbns-l2r primorial', \
	"dbns_pre_list_s128_hi.dat" with lines title 'dbns-pre prime power'

set terminal png 
set output 'winners-mpz.png'
set title "Timings for mpz prime powers" 
set xlabel "Bound for Primorial" 
set ylabel "Relative timing" 
set autoscale
plot "dbns_l2r_mpz.dat" with lines title 'dbns-l2r primorial', \
	"dbns_pre_list_mpz.dat" with lines title 'dbns-pre prime power'



#######
# 256 #
#######

set terminal png 
set output 'l2r_256.png'
set title "Timings for l2r with a bound of 256" 
set xlabel "Value of max_a" 
set ylabel "Relative timing" 
set autoscale
plot "unit_l2r_256.dat" with lines title 'unit', \
	"mpz_l2r_256.dat" with lines title 'mpz_l2r', \
	"s64_l2r_256.dat" with lines title 's64_l2r', \
	"s128_lo_l2r_256.dat" with lines title 's128_lo_l2r', \
	"s128_hi_l2r_256.dat" with lines title 's128_hi_l2r'

set terminal png 
set output 'l2r_256-zoom.png'
set title "Timings for l2r with a bound of 256" 
set xlabel "Value of max_a" 
set ylabel "Relative timing" 
set xrange [300:]
plot "mpz_l2r_256.dat" with lines title 'mpz_l2r', \
	"s64_l2r_256.dat" with lines title 's64_l2r', \
	"s128_lo_l2r_256.dat" with lines title 's128_lo_l2r', \
	"s128_hi_l2r_256.dat" with lines title 's128_hi_l2r'


#######
# 512 #
#######

set terminal png 
set output 'l2r_512.png'
set title "Timings for l2r with a bound of 512" 
set xlabel "Value of max_a" 
set ylabel "Relative timing" 
set autoscale
plot "unit_l2r_512.dat" with lines title 'unit_l2r', \
	"mpz_l2r_512.dat" with lines title 'mpz_l2r', \
	"s64_l2r_512.dat" with lines title 's64_l2r', \
	"s128_lo_l2r_512.dat" with lines title 's128_lo_l2r', \
	"s128_hi_l2r_512.dat" with lines title 's128_hi_l2r'


set terminal png 
set output 'l2r_512-zoom.png'
set title "Timings for l2r with a bound of 512" 
set xlabel "Value of max_a" 
set ylabel "Relative timing" 
set xrange [650:]
plot "mpz_l2r_512.dat" with lines title 'mpz_l2r', \
	"s64_l2r_512.dat" with lines title 's64_l2r', \
	"s128_lo_l2r_512.dat" with lines title 's128_lo_l2r', \
	"s128_hi_l2r_512.dat" with lines title 's128_hi_l2r'



########
# 1024 #
########

set terminal png 
set output 'l2r_1024.png'
set title "Timings for l2r with a bound of 1024" 
set xlabel "Value of max_a" 
set ylabel "Relative timing" 
set autoscale
plot "unit_l2r_1024.dat" with lines title 'unit_l2r', \
	"mpz_l2r_1024.dat" with lines title 'mpz_l2r', \
	"s64_l2r_1024.dat" with lines title 's64_l2r', \
	"s128_lo_l2r_1024.dat" with lines title 's128_lo_l2r', \
	"s128_hi_l2r_1024.dat" with lines title 's128_hi_l2r'


set terminal png 
set output 'l2r_1024-zoom.png'
set title "Timings for l2r with a bound of 1024" 
set xlabel "Value of max_a" 
set ylabel "Relative timing" 
set xrange [1300:]
plot "mpz_l2r_1024.dat" with lines title 'mpz_l2r', \
	"s64_l2r_1024.dat" with lines title 's64_l2r', \
	"s128_lo_l2r_1024.dat" with lines title 's128_lo_l2r', \
	"s128_hi_l2r_1024.dat" with lines title 's128_hi_l2r'



########
# 2048 #
########

set terminal png 
set output 'l2r_2048.png'
set title "Timings for l2r with a bound of 2048" 
set xlabel "Value of max_a" 
set autoscale
set ylabel "Relative timing" 
plot "unit_l2r_2048.dat" with lines title 'unit_l2r', \
	"mpz_l2r_2048.dat" with lines title 'mpz_l2r', \
	"s64_l2r_2048.dat" with lines title 's64_l2r', \
	"s128_lo_l2r_2048.dat" with lines title 's128_lo_l2r', \
	"s128_hi_l2r_2048.dat" with lines title 's128_hi_l2r'



set terminal png 
set output 'l2r_2048-zoom.png'
set title "Timings for l2r with a bound of 2048" 
set xlabel "Value of max_a" 
set xrange [1800:2950]
set ylabel "Relative timing" 
plot "mpz_l2r_2048.dat" with lines title 'mpz_l2r', \
	"s64_l2r_2048.dat" with lines title 's64_l2r', \
	"s128_lo_l2r_2048.dat" with lines title 's128_lo_l2r', \
	"s128_hi_l2r_2048.dat" with lines title 's128_hi_l2r'


set terminal png 
set output 'l2r_2048-zoom2.png'
set title "Timings for l2r with a bound of 2048" 
set xlabel "Value of max_a" 
set xrange [2600:2950]
set ylabel "Relative timing" 
plot "mpz_l2r_2048.dat" with lines title 'mpz_l2r', \
	"s64_l2r_2048.dat" with lines title 's64_l2r', \
	"s128_lo_l2r_2048.dat" with lines title 's128_lo_l2r', \
	"s128_hi_l2r_2048.dat" with lines title 's128_hi_l2r'



