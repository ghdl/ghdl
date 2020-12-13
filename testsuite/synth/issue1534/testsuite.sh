#! /bin/sh

. ../../testenv.sh

for f in ent ent2 xor01 xor02; do
  synth_analyze $f
done

clean

echo "Test successful"
