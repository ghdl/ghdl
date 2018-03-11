#! /bin/sh

. ../../testenv.sh

analyze -Wno-library dff.vhdl
elab_failure dff

clean

echo "Test successful"
