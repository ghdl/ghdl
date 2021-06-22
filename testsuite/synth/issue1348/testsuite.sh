#! /bin/sh

. ../../testenv.sh

synth_tb sdp_simple 2> sdp_simple.log
grep "found RAM" sdp_simple.log

echo "Test successful"
