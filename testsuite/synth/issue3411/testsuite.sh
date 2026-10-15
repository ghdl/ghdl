#! /bin/sh

. ../../testenv.sh

synth --out=raw --latches latch1.vhdl -e > syn_latch1.raw

fgrep dlatch syn_latch1.raw
fgrep dff syn_latch1.raw

synth --out=none --latches --stats repro4.vhdl -e > repro4.stat
fgrep "dff: 1" repro4.stat
fgrep "adff: 2" repro4.stat
fgrep "dlatch: 1" repro4.stat

synth --out=none --stats --latches wishbone_pkg.vhdl top.vhdl -e > top.stat
fgrep "dff: 1" top.stat
fgrep "adff: 2" top.stat
fgrep "dlatch: 1" top.stat


clean

echo "Test successful"
