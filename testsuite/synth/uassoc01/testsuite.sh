#! /bin/sh

. ../../testenv.sh

for t in uassoc01 uassoc02 uassoc03; do
    synth_tb $t
done

echo "Test successful"
