#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    for f in bug_tb bug_tb3 bug_tb4 bug_tb5; do
	analyze $f.vhdl
	elab_simulate ghdl_bug_tb
	clean
    done
fi

echo "Test successful"
