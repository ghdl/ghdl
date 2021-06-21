#! /bin/sh

. ../../testenv.sh

for t in memmux04; do
    synth_tb $t 2> $t.log
    grep "found R" $t.log
done

for t in dpram1r dpram2r dpram2w; do
    synth_tb $t 2> $t.log
done

echo "Test successful"
