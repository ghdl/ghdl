#! /bin/sh

. ../../testenv.sh

synth rawfile_pkg.vhd rom16.vhd -e > syn_rom16.vhdl

echo "Test successful"
