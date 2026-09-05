#! /bin/sh

. ../../testenv.sh

# Simulating a design containing an if-generate with an else branch used
# to crash as soon as a wave dumper was enabled, and on Windows the crash
# surfaced only as "exception raised: unknown reason", which is what
# issue2862 reports; the reporter traced it to issue2640, whose
# reproducer this is.  It runs cleanly now, so check every dumper on
# every backend to keep it that way.
export GHDL_STD_FLAGS="--std=08"

analyze mwe.vhd

for opt in --wave=w.ghw --vcd=w.vcd --fst=w.fst; do
  elab_simulate ent1 $opt
  rm -f w.ghw w.vcd w.fst
done

clean

echo "Test successful"
