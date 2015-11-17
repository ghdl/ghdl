#! /bin/sh

. ../../testenv.sh

analyze er_pack.vhd
analyze pp_fir_filter.vhd
elab_simulate pp_fir_filter

clean

echo "Test successful"
