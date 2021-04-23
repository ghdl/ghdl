#! /bin/sh

. ../../testenv.sh

for t in aggr01 aggr02 aggr03; do
    synth_tb $t
done

echo "Test successful"
