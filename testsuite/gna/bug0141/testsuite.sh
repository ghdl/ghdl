#! /bin/sh

. ../../testenv.sh

analyze LFSR_segfault.vhdl
elab LFSR_segfault
simulate LFSR_segfault
simulate LFSR_segfault -gmaxloop=470000

clean

echo "Test successful"
