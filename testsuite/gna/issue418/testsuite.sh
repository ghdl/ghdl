#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab repro

if ghdl_has_feature repro ghw; then
  simulate repro --wave=repro.ghw
  # How to test the ghw ?  Use ghwdump ?
fi

analyze repro2.vhdl
elab repro2

if ghdl_has_feature repro2 ghw; then
  simulate repro2 --wave=repro2.ghw
  # How to test the ghw ?  Use ghwdump ?
fi

clean
rm -f repro.ghw repro2.ghw

echo "Test successful"
