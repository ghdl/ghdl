#! /bin/sh

. ../../testenv.sh

for t in insert01 insert02; do
    synth_tb $t
done

echo "Test successful"
