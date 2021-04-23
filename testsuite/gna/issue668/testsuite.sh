#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

for f in wb_demux_tb repro2; do
	analyze "$f.vhdl"
	elab "$f"

	if ghdl_has_feature "$f" ghw; then
	  simulate "$f" --dump-rti
	  simulate "$f" --wave="$f".ghw
	  ghw_diff "$f"
	  rm -f "$f.ghw" "$f.txt"
	fi
done

clean

echo "Test successful"
