#! /bin/sh

. ../../testenv.sh

analyze sample_slice_ports.vhdl

analyze repro2.vhdl
elab_simulate sliced_ex

clean

echo "Test successful"
