#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate ent
clean

if ! cmp file.txt file.raw; then
  echo "Comparison mismatch"
  exit 1
fi

rm -f file.txt file.raw

echo "Test successful"
