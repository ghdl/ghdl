#! /bin/sh

. ../../testenv.sh

files="
gcrash-1a.vhdl
gcrash-6a.vhdl
gcrash-9a.vhdl
"

export GHDL_STD_FLAGS=--std=08
for f in $files; do
    analyze_failure $f
done

clean

echo "Test successful"
