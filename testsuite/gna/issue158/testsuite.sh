#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
if ghdl_has_feature repro ghw; then
  elab_simulate repro --wave=repro.ghw
  # How to test the ghw ?  Use ghwdump ?
fi
clean
rm -f repro.ghw

echo "Test successful"
