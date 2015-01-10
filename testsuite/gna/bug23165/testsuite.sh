#! /bin/sh

. ../../testenv.sh

analyze mwe_failing/counter.vhd
analyze mwe_failing/mwe.vhd
elab_simulate mwe --stop-time=100us

analyze mwe_working/counter.vhd
analyze mwe_working/mwe.vhd
elab_simulate mwe --stop-time=100us --wave=output.ghw

analyze mwe2/mwe.vhd
elab_simulate mwe --stop-time=100us

clean

echo "Test successful"
