#! /bin/sh

. ../../testenv.sh

analyze nullacc.vhdl
elab_simulate_failure nullacc

analyze fileerr.vhdl
elab_simulate_failure fileerr

clean

echo "Test successful"
