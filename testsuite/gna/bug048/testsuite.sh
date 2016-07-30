#! /bin/sh

. ../../testenv.sh

analyze leftof1.vhdl
elab_simulate leftofrightof

analyze leftof2.vhdl
elab_simulate leftofrightof

analyze leftof3.vhdl
elab_simulate leftofrightof

clean

echo "Test successful"
