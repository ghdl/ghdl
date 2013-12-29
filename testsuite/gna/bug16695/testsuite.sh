#! /bin/sh

. ../../testenv.sh

analyze --ieee=synopsys lfsr_updown.vhd lfsr_updown_tb.vhd
elab_simulate --ieee=synopsys lfsr_updown_tb --stop-time=200ns

clean

echo "Test successful"
