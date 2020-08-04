#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

for f in top; do
  analyze $f.vhdl
  elab $f

  if ghdl_has_feature $f ghw; then
    simulate $f --dump-rti
    simulate $f --wave=$f.ghw
    rm -f $f.ghw
  fi
done

clean

echo "Test successful"
