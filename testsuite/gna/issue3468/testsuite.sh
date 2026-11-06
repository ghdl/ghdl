#! /bin/sh

. ../../testenv.sh

#  Reprint shows the delimited comments attached to each node, as written.
"$GHDL" --reprint --std=08 delimited.vhdl > delimited.out
diff delimited.ref delimited.out

#  A line break inside a delimited comment is reprinted the same, whether the
#  file stores it as LF, CR+LF or CR.
tr -d '\r' < delimited.vhdl > delimited_lf.vhdl
awk '{ printf "%s\r\n", $0 }' delimited_lf.vhdl > delimited_crlf.vhdl
tr '\n' '\r' < delimited_lf.vhdl > delimited_cr.vhdl
for f in delimited_lf delimited_crlf delimited_cr; do
  "$GHDL" --reprint --std=08 $f.vhdl > delimited.out
  diff delimited.ref delimited.out
done

rm -f delimited.out delimited_lf.vhdl delimited_crlf.vhdl delimited_cr.vhdl

#  Delimited comments are not allowed before vhdl 2008.
analyze_failure --std=93 delimited.vhdl

clean

echo "Test successful"
