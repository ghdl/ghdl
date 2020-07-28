#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="-fpsl --std=02"

analyze psl_test_error.vhd
elab_simulate psl_test_error --stop-time=200ns

analyze psl_test_working.vhd
elab_simulate psl_test_working --stop-time=200ns

clean
