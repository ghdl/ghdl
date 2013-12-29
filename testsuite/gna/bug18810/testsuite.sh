#! /bin/sh

. ../../testenv.sh

analyze DMEM.vhd OISC_SUBLEQ.vhd BENCH_OISC_SUBLEQ.vhd
elab_simulate BENCH_OISC_SUBLEQ --stop-time=2us

clean

echo "Test successful"
