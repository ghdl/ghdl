#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 -fpsl"

analyze cover_report.vhd
elab_simulate cover_report
clean

echo "Test successful"
