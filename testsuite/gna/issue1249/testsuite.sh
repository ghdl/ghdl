#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
elab repro1
if ghdl_has_feature repro1 dump-rti; then
    simulate repro1 --dump-rti
fi

clean

echo "Test successful"
