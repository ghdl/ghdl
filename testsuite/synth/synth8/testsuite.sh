#! /bin/sh

. ../../testenv.sh

for t in vector8_test1 test5; do
    synth_tb $t
done

echo "Test successful"
