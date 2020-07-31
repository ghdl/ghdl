#! /bin/sh

. ../../testenv.sh

synth -fpsl --no-assert-cover cover_overlap_misparse.vhdl -e > syn_nocover.vhdl
! grep -q cover syn_nocover.vhdl

synth -fpsl cover_overlap_misparse.vhdl -e > syn_cover.vhdl
grep -q cover syn_cover.vhdl

clean

echo "Test successful"
