#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab repro

analyze repro2.vhdl
elab repro2

analyze tc749.vhdl
elab tc749

if ghdl_has_feature repro2 ghw; then
  simulate repro --wave=repro.ghw
  simulate repro2 --wave=repro2.ghw
  simulate tc749 --wave=tc749.ghw
  # How to test the ghw ?  Use ghwdump ?
fi

analyze repro3.vhdl
elab_simulate repro3

clean
rm -f repro.ghw repro2.ghw tc749.ghw

echo "Test successful"
