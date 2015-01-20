#! /bin/sh

. ../../testenv.sh

for i in 1 2 3 4 5 6 7 8 9 10 11 12 13; do
  analyze paren$i.vhdl
  elab_simulate paren$i
done

clean

echo "Test successful"
