#! /bin/sh

. ../../testenv.sh

synth_analyze repro3

# Look for a dff.
grep -q "if rising_edge (wrap_clk) then" syn_repro3.vhdl
clean

echo "Test successful"
