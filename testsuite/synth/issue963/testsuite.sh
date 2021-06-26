#! /bin/sh

. ../../testenv.sh

for t in ent ent2; do
    synth_tb $t
done

echo "Test successful"
