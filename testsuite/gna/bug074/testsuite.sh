#! /bin/sh

. ../../testenv.sh

# Not a real test, should check for leaks.
analyze repro.vhdl
elab_simulate repro --stop-time=1us

clean

echo "Test successful"
