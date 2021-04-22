#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

for item in repro repro2 tc749; do
  analyze "$item".vhdl
  elab "$item"
  if ghdl_has_feature "$item" ghw; then
    simulate "$item" --wave="$item".ghw
    ghw_diff "$item"
    rm -f "$item".txt "$item".ghw
  fi
done

analyze repro3.vhdl
elab_simulate repro3

clean

echo "Test successful"
