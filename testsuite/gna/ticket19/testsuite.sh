#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="-fpsl --std=02"

analyze psl_test_cover.vhd
elab_simulate psl_test_cover --stop-time=200ns

analyze psl_test_cover2.vhd
elab_simulate psl_test_cover2 --stop-time=200ns

analyze psl_test_cover3.vhd
elab_simulate psl_test_cover3 --stop-time=200ns

clean
