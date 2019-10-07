#! /bin/sh

. ../../testenv.sh

synth sample_pkg.vhdl pkg_test.vhdl -e pkg_test > syn_pkg_test.vhdl

echo "Test successful"
