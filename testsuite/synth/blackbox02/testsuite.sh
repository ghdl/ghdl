#! /bin/sh

. ../../testenv.sh

# Using syn_black_box attribute
verilog_synth_tb my_or3 my_or_bb.v

# Using -lib/-no-lib
synth my_or3.v -lib my_or.v -no-lib -e > syn_my_or3.v
if fgrep -q " of my_or is" syn_my_or3.v; then
    exit 1;
fi
echo "Test successful"
