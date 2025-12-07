#! /bin/sh

. ../../testenv.sh

for t in asgn01 asgn02 asgn03 asgn04 asgn05 asgn06 asgn07 asgn08 \
	 arr04 asgn09; do
    synth_tb $t
done

GHDL_SYNTH_FLAGS=--latches
synth_tb asgn10

echo "Test successful"
