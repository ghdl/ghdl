#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro --backtrace-severity=warning

clean

echo "Test successful"
