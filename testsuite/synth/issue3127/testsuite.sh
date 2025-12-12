#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze --work=gaisler pkg.vhd
analyze top.vhd

# Component of an external name is not bound
synth_failure --keep-hierarchy=no top -e

analyze --work=gaisler entityA.vhd
if ghdl_is_preelaboration; then
  elab_simulate top
fi

synth --keep-hierarchy=no --out=verilog top > reprod.v

clean
clean gaisler

echo "Test successful"
