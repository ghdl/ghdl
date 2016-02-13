#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 -fpsl"

analyze cover_report2.vhd
elab_simulate cover_report2

# Submitted case was not correct.
analyze_failure cover_report.vhd

# Parenthesis not allowed in sequences.
analyze_failure cover_report3.vhd

analyze cover_report1.vhd
elab_simulate cover_report1
clean

echo "Test successful"
