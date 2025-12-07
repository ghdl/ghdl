#! /bin/sh

. ../../testenv.sh

for t in conv01 pos01; do
    synth_analyze $t
    clean
done

synth_only convacc01

synth_failure convarr01.vhdl -e
synth_failure convflt01.vhdl -e
synth_failure convflt02.vhdl -e
synth_failure convflt03.vhdl -e

GHDL_STD_FLAGS=--std=08
synth_failure convrec01.vhdl -e
synth_only convarr02

synth_only convarr03
synth_only convarr04
synth_failure convarr05.vhdl -e

synth_only convrec02


echo "Test successful"
