#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate bug > out.txt 2> err.txt
diff_nocr -q out.txt out.ref

rm -f out.txt err.txt
clean

echo "Test successful"
