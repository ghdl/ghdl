#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab wb_demux_tb

if ghdl_has_feature wb_demux_tb ghw; then
  simulate wb_demux_tb --dump-rti
  simulate wb_demux_tb --wave=w.ghw
fi

analyze repro2.vhdl
elab repro2
if ghdl_has_feature repro2 ghw; then
  simulate repro2 --dump-rti
  simulate repro2 --wave=w.ghw
fi

clean

rm -f w.ghw

echo "Test successful"
