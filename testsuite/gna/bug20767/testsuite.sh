#! /bin/sh

. ../../testenv.sh

analyze_failure xilname.vhdl

analyze -frelaxed-rules xilname.vhdl

analyze aggr.vhdl
elab_simulate tb

clean

echo "Test successful"
