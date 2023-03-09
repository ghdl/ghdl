#! /bin/sh

. ../../testenv.sh

for t in case01 case02 case03 case04; do
    synth_tb $t
done

for t in case05 case06 case07; do
  synth_only $t
done

echo "Test successful"
