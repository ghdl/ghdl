#! /bin/sh

. ../../testenv.sh

analyze foo.vhdl
elab_simulate foo

analyze tb.vhdl
# elab_simulate testbenchautomated --ieee-asserts=disable --stop-time=100us
clean

echo "Test successful"
