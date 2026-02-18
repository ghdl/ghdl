#! /bin/sh

. ../../testenv.sh

for t in repro3; do
    synth_tb $t
done

clean

echo "Test successful"
