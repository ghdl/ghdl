#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth -gG_CACHE_SIZE=8 -gG_ADDRESS_SIZE=4 -gG_DATA_SIZE=8 avm_cache.vhd avm_cache.psl -e avm_cache > syn_avm_cache.vhdl

echo "Test successful"
