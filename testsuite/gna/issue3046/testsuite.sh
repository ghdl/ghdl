#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if ghdl_is_preelaboration; then
  for f in a b c d e; do
    analyze $f.vhdl
    elab_simulate ent
    clean
  done

  analyze --force-analysis e.vhdl
  elab_simulate --force-analysis ent

  analyze rec_aggr.vhdl
  elab_simulate rec_aggr

  analyze rec_assoc.vhdl
  elab_simulate rec_assoc

  analyze recmat_aggr.vhdl
  elab_simulate recmat_aggr

  analyze recmat_assoc.vhdl
  elab_simulate recmat_assoc

  analyze recrec_assoc.vhdl
  elab_simulate recrec_assoc
fi

echo "Test successful"
