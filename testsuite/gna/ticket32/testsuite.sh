#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

clean

analyze --work=unisim muxcy.vhdl
analyze --work=poc simulation.vhdl
analyze --work=poc arith_prefix_and.vhdl
analyze --work=test arith_prefix_and_tb.vhdl
elab_simulate --work=test arith_prefix_and_tb

clean unisim
clean poc
clean test

echo "Test successful"
