#! /bin/sh

. ../../testenv.sh

synth --work=lib_a entityA.vhdl --work=lib_b entityB.vhdl --work=work wrapperA.vhdl wrapperB.vhdl top.vhdl -e > syn_top.vhdl
analyze syn_top.vhdl

echo "Test successful"

