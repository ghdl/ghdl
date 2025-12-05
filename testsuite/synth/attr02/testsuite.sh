#! /bin/sh

. ../../testenv.sh

for t in succ01 pred01 leftof01 rightof01 left01; do
    synth_tb $t
done

GHDL_STD_FLAGS=--std=08
synth_tb right01

echo "Test successful"
