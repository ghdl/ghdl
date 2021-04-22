#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze wishbone_pkg-2008.vhd
analyze wb_demux.vhd
analyze wb_demux_tb.vhd
elab wb_demux_tb
if ghdl_has_feature wb_demux_tb ghw; then
  simulate wb_demux_tb --wave=w.ghw
  ghw_diff w
  rm -f w.txt w.ghw
fi

clean

echo "Test successful"
