#! /bin/sh

. ../../testenv.sh

for t in subprg01 subprg02; do
    synth_tb $t
done

echo "Test successful"
