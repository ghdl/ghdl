#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
for item in wb_demux_tb repro2; do
  analyze "$item".vhdl
  elab "$item"

  if ghdl_has_feature "$item" ghw; then
    elab_simulate "$item" --dump-rti
    elab_simulate "$item" --wave="$item".ghw
    ghw_diff "$item"
    rm -f "$item".txt "$item".ghw
  fi
done

clean

echo "Test successful"
