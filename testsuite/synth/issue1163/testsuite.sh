#! /bin/sh

. ../../testenv.sh

for f in bug bug2 bug3; do
  synth_analyze $f
  clean
done

echo "Test successful"
