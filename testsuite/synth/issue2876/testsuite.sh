#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_tb record_bug

synth_tb record_bug2

synth_tb record_bug3

do_bug4()
{
    i=$1
    j=$2

    analyze record_bug4.vhdl tb_record_bug4.vhdl
    elab_simulate tb_record_bug4 -gNEL1=$j -gNEL2=$i
    clean

    synth -gNEL1=$j -gNEL2=$i record_bug4.vhdl -e > syn_record_bug4_$i_$j.vhdl
    analyze syn_record_bug4_$i_$j.vhdl tb_record_bug4.vhdl
    elab_simulate tb_record_bug4 -gNEL2=$i --ieee-asserts=disable-at-0 --assert-level=error
    clean
}

do_bug4 2 2
do_bug4 4 3
do_bug4 3 4
do_bug4 5 7
do_bug4 8 4
do_bug4 3 8

echo "Test successful"
