#! /bin/sh

. ../../testenv.sh

analyze dispgen.vhdl
elab_simulate dispgen

elab_simulate dispgen -gstr=Hello

analyze genint.vhdl
elab_simulate_failure genint

simulate genint -gVAL=9
simulate genint -gVal=-159

simulate_failure genint -gval=200

clean

echo "Test successful"
