#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab wb_demux_tb

if ghdl_has_feature wb_demux_tb ghw; then
  simulate wb_demux_tb --trace-signals | 
    grep wb_demux_tb | cut -d ' ' -f 1,3- > tb.out
  diff_nocr tb.out tb.ref
fi

rm -f tb.out
clean

echo "Test successful"

