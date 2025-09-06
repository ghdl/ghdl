#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if ghdl_is_preelaboration; then
  for f in a b c d; do
    analyze $f.vhdl
    elab_simulate ent
    clean
  done

  analyze rec_aggr.vhdl
  elab_simulate rec_aggr

  analyze recmat_aggr.vhdl
  elab_simulate recmat_aggr
fi

echo "Test successful"
