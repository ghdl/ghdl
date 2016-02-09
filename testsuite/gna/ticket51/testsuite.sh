#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab_simulate ent

if "$GHDL" -v | egrep -q 'mcode|interpret'; then
  echo "Option -o not supported by mcode/interpretation"
else
  "$GHDL" --elab-run -o ent1 ent
  "$GHDL" --elab-run -o "$PWD/ent2" ent
fi

rm -f ent1 ent2
clean

echo "Test successful"
