#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab_simulate ent

if $GHDL -v | grep -q mcode; then
  echo "-o not supported by mcode"
else
  $GHDL --elab-run -o ent1 ent
  $GHDL --elab-run -o $PWD/ent2 ent
fi

rm -f ent1 ent2
clean

echo "Test successful"
