#! /bin/sh

. ../../testenv.sh

for t in repro_slv repro_uns repro_sgn repro_nat repro_rng1; do
    synth_tb $t
done

echo "Test successful"
