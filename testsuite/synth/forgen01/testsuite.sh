#! /bin/sh

. ../../testenv.sh

for t in forgen01 forgen02 forgen03; do
    synth_tb $t
done

echo "Test successful"
