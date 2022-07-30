#! /bin/sh

. ../../testenv.sh

analyze e.vhdl

for u in e1 e2; do
  analyze $u.vhdl
  elab_simulate $u
done

clean

echo "Test successful"
