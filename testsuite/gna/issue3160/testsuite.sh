#! /bin/sh

. ../../testenv.sh

# A record aggregate whose elements are named in the reverse of their
# declaration order, built by a function returning a record with unbounded
# elements.  Translate_Record_Aggregate_Bounds used to assert that a named
# choice was at the position of the running counter, so this ended in
#
#   raised ADA.ASSERTIONS.ASSERTION_ERROR : trans-chap7.adb:4379
#
# on the code-generating backends.  Verified against GHDL 5.1.1, the version
# #3160 was reported on.

export GHDL_STD_FLAGS=--std=08

analyze repro.vhdl
elab_simulate repro

clean

echo "Test successful"
