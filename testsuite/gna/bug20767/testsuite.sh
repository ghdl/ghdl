#! /bin/sh

. ../../testenv.sh

analyze_failure --std=93 xilname.vhdl

analyze -frelaxed-rules xilname.vhdl

analyze aggr.vhdl
elab_simulate tb

clean

echo "Test successful"
