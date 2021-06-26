#! /bin/sh

. ../../testenv.sh

# accum
synth_tb accum pkg.vhdl

# accumwr
synth_tb accumwr pkg.vhdl accum.vhdl

# patgen
synth_tb patgen pkg.vhdl

# patacc
synth_tb patacc pkg.vhdl patgen.vhdl

echo "Test successful"
