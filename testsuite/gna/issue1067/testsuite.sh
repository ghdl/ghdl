#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze top.vhdl
elab top

if ghdl_has_feature top ghw; then
  simulate top --dump-rti
  simulate top --wave=top.ghw
  ghw_diff "top"
  rm -f "top".txt "top".ghw
fi

clean

echo "Test successful"
