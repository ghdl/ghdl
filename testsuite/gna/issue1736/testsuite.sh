#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze --work=mylib fifo.vhdl
analyze ent.vhdl
cmd="$GHDL elab-order --libraries $GHDL_STD_FLAGS ent"
echo "run $cmd"
eval "$cmd" > ent.out
diff_nocr ent.ref ent.out

clean
clean mylib
rm -f ent.out

echo "Test successful"
