#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--ieee=synopsys
analyze definitions.vhdl
analyze alu.vhdl
analyze basicblocks.vhdl
analyze tb-alu.vhdl
elab_simulate tb_alu --stop-time=50ns

clean

echo "Test successful"
