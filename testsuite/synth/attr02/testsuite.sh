#! /bin/sh

. ../../testenv.sh

for t in succ01 pred01 leftof01 rightof01; do
    synth_tb $t
done

echo "Test successful"
