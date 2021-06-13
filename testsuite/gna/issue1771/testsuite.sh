#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tf.vhdl
elab_simulate tf

analyze add_carry_ghdl_testbench.vhdl
elab_simulate add_carry_ghdl_testbench > add_carry_testbench.out
diff_nocr add_carry_testbench.ref add_carry_testbench.out

clean

echo "Test successful"
