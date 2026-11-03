#! /bin/sh

. ../../testenv.sh

#  Reprint shows the comments attached to each declaration and statement.
"$GHDL" --reprint --std=19 gather.vhdl > gather.out
diff gather.ref gather.out

rm -f gather.out

echo "Test successful"
