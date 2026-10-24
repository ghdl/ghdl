#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
for f in format_n_analysis format_n_runtime format_p_disclosure; do
  analyze $f.vhdl
  elab_simulate $f
done

clean

echo "Test successful"
