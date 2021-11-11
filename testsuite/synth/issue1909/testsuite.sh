#! /bin/sh

. ../../testenv.sh

for t in reproducebug; do
    synth_tb $t
done

echo "Test successful"
