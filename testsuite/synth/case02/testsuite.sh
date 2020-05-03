#! /bin/sh

. ../../testenv.sh

for t in case01 case02 case03; do
    synth_tb $t
done

echo "Test successful"
