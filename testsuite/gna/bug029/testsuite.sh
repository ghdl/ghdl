#! /bin/sh

. ../../testenv.sh

analyze fft.vhdl
analyze fft1.vhdl
analyze fft2.vhdl
analyze repro1.vhdl
analyze repro2.vhdl
#elab_simulate simple1

clean

echo "Test successful"
