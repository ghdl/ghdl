#! /bin/sh

. ../../testenv.sh

$GHDL -i lib_numeric_tb.vhd
$GHDL -m numeric_tb
simulate numeric_tb --stop-time=10ns --wave=numeric_tb.ghw \
 --sdf=typ==lib_numeric_tb.sdf

clean
rm numeric_tb.ghw

echo "Test successful"
