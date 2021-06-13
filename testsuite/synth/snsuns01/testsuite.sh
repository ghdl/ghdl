#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="-fsynopsys -fexplicit"

# Compare opers.
for s in u s; do
    for f in adds subs unaries muls cmplt cmple cmpgt cmpge cmpeq cmpne shrs; do
        analyze $s$f.vhdl
        analyze tb_$f.vhdl
        elab_simulate tb_$f > $s$f.ref

        synth $s$f.vhdl -e > syn_$s$f.vhdl
        analyze tb_$f.vhdl
        elab_simulate tb_$f > $s$f.out

        diff_nocr $s$f.out $s$f.ref
    done

  clean
done


echo "Test successful"
