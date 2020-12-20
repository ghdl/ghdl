#! /bin/sh

. ../../testenv.sh

for f in ent1 ent2 ; do
  synth_analyze $f
done

clean

echo "Test successful"
