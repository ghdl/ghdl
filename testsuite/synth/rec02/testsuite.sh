#! /bin/sh

. ../../testenv.sh

synth_tb rec02 pkg_rec02.vhdl

# Verify that nested record fields produce individual leaf-level signals
# with chained bracket identifiers (\p[i][a]\, \p[i][b]\) rather than
# a single packed vector (\p[i]\).
synth pkg_rec02.vhdl rec02.vhdl -e rec02 > syn_rec02_check.vhdl
if ! fgrep -q '\wrap_p[i][a]\' syn_rec02_check.vhdl; then
  echo "FAIL: expected chained bracket signal \\wrap_p[i][a]\\ not found"
  exit 1
fi
if ! fgrep -q '\wrap_p[i][b]\' syn_rec02_check.vhdl; then
  echo "FAIL: expected chained bracket signal \\wrap_p[i][b]\\ not found"
  exit 1
fi
if fgrep -q 'typ_wrap_p[i]' syn_rec02_check.vhdl; then
  echo "FAIL: packed sub-record type typ_wrap_p[i] still present"
  exit 1
fi

clean

echo "Test successful"
