#! /bin/sh

. ../../testenv.sh

for t in if01 if02; do
    synth_tb $t
done

echo "Test successful"
