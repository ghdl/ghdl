#! /bin/sh

. ../../testenv.sh

#  Reprint shows the comments attached to each statement, including those of
#  the statements replaced by the analysis.
"$GHDL" --reprint --std=19 stmts.vhdl > stmts.out
diff stmts.ref stmts.out

rm -f stmts.out

echo "Test successful"
