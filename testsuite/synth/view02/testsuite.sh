#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=19

synth_tb view02 pkg_view02.vhdl

# Verify that a nested record field under a mode view produces individual
# leaf-level signals (\wrap_m[data][a]\, \wrap_m[data][b]\) rather than
# a packed sub-record vector (\wrap_m[data]\).
synth pkg_view02.vhdl view02.vhdl -e view02 > syn_view02_check.vhdl
if ! fgrep -q '\wrap_m[data][a]\' syn_view02_check.vhdl; then
  echo "FAIL: expected chained bracket signal \\wrap_m[data][a]\\ not found"
  exit 1
fi
if ! fgrep -q '\wrap_m[data][b]\' syn_view02_check.vhdl; then
  echo "FAIL: expected chained bracket signal \\wrap_m[data][b]\\ not found"
  exit 1
fi
if fgrep -q 'typ_wrap_m[data]' syn_view02_check.vhdl; then
  echo "FAIL: packed sub-record type typ_wrap_m[data] still present"
  exit 1
fi

analyze view03_pkg.vhdl view03.vhdl tb_view03.vhdl
elab_simulate tb_view03
clean

synth view03_pkg.vhdl view03.vhdl -e > syn_view03.vhdl
analyze view03_pkg.vhdl syn_view03.vhdl tb_view03.vhdl
elab_simulate tb_view03
clean

analyze view04_pkg.vhdl view04.vhdl tb_view04.vhdl
elab_simulate tb_view04
clean

synth view04_pkg.vhdl view04.vhdl -e > syn_view04.vhdl
analyze view04_pkg.vhdl syn_view04.vhdl tb_view04.vhdl
elab_simulate tb_view04
clean

echo "Test successful"
