#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
for f in repro1 recordofrecord_tb; do
  analyze "$f".vhdl
  elab "$f"

  if ghdl_has_feature "$f" ghw; then
    simulate "$f" --dump-rti
    simulate "$f" --wave="$f".ghw
    ghw_diff "$f"
    rm -f "$f".txt "$f".ghw
  fi
done

clean

echo "Test successful"
