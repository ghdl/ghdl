#! /bin/sh

. ../../testenv.sh

analyze_failure -Werror=runtime-error repro1.vhdl

analyze repro2.vhdl
elab_simulate_failure repro2 | tee repro2.err
grep "1 downto 0" repro2.err

analyze repro3.vhdl
elab_simulate_failure repro3 | tee repro3.err
grep "1 downto 0" repro3.err

GHDL_STD_FLAGS=--std=08
analyze direction_mismatch.vhd  tb_direction_mismatch.vhd
elab_simulate_failure  tb_direction_mismatch
clean

echo "Test successful"
