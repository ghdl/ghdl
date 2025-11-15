#! /bin/sh

. ../../testenv.sh

synth --work=lib_a entityA.vhdl --work=lib_b entityB.vhdl --work=work wrapperA.vhdl wrapperB.vhdl top.vhdl -e > syn_top.vhdl
analyze syn_top.vhdl

synth --out=verilog entityA.vhdl -e > syn_entitya.v
if fgrep -q ' do' syn_entitya.v ; then
    exit 1
fi

echo "Test successful"

