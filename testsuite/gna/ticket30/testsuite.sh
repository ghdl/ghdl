#! /bin/sh

. ../../testenv.sh

# Don't use grep -q, doesn't work on windows.

if ! "$GHDL" --dir ieee | grep vital > /dev/null; then
  echo "No vital libraries, skipping"
  exit 0
fi

"$GHDL" -i lib_numeric_tb.vhd
"$GHDL" -m numeric_tb
if ghdl_has_feature numeric_tb ghw; then
  simulate numeric_tb --stop-time=10ns --wave=numeric_tb.ghw \
   --sdf=typ==lib_numeric_tb.sdf
fi

clean
rm -f numeric_tb.ghw

echo "Test successful"
