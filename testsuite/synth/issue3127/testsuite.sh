#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze --work=gaisler pkg.vhd
analyze --work=gaisler entityA.vhd
analyze top.vhd
if ghdl_is_preelaboration; then
  elab_simulate top
fi

synth --keep-hierarchy=no --out=verilog top > reprod.v

clean
clean gaisler

echo "Test successful"
