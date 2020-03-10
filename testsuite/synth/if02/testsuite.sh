#! /bin/sh

. ../../testenv.sh

for t in if01 if02 if03; do
    synth_failure $t.vhdl -e
done

echo "Test successful"
