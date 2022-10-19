#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys

# Compare opers.
for f in adds subs unaries muls cmplt cmple cmpgt cmpge cmpeq cmpne shrs exts; do
    analyze $f.vhdl
    analyze tb_$f.vhdl
    elab_simulate tb_$f > $f.ref

    synth $f.vhdl -e > syn_$f.vhdl
    analyze tb_$f.vhdl
    elab_simulate tb_$f > $f.out

    diff_nocr $f.out $f.ref
done

for t in sns01; do
    synth $t.vhdl -e $t > syn_$t.vhdl
    # No analysis because of conflict between numeric_std.unsigned and
    # std_logic_arith.unsigned
#    analyze syn_$t.vhdl
    clean
done

synth_tb reduce
synth_tb add03

echo "Test successful"
