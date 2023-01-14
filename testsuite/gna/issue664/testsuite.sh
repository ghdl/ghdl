#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb_pkg.vhdl
analyze numeric_system_pkg.vhdl
analyze reset_synchronizer.vhdl
analyze quire_accumulator.vhdl
analyze quire_accumulator_tb.vhdl
elab quire_accumulator_tb

simulate quire_accumulator_tb

# if ghdl_has_feature quire_accumulator_tb ghw; then
#   simulate quire_accumulator_tb --wave=w.ghw
# fi

rm -f w.ghw
clean

echo "Test successful"

