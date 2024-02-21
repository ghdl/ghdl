#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if ghdl_is_preelaboration; then
    analyze tb.vhdl
    elab_simulate tb --fst=tb.fst

    if fstminer -h > /dev/null; then
	echo "Check with fstminer..."
	fstminer -d tb.fst -m 0 -n > miner.out
	diff_nocr miner.ref miner.out
    fi
    clean
fi


echo "Test successful"
