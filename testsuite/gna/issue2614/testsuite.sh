#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if ghdl_is_preelaboration; then
    analyze recursive_xor.vhdl recursive_xor_tb.vhdl
    elab_simulate recursive_xor_tb --fst=tb.fst

    if fstminer -h > /dev/null; then
	echo "Check with fstminer..."
	fstminer -d tb.fst -m 01 | grep xi > miner.out
	diff_nocr miner.ref miner.out
    fi
    clean
fi


echo "Test successful"
