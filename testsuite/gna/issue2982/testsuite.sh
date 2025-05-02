#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze nfmac.vhdl
analyze caseg.vhdl
if ghdl_has_feature nfmac dump-rti; then
    elab_simulate nfmac --dump-rti
    elab_simulate caseg --dump-rti
fi

clean

echo "Test successful"
