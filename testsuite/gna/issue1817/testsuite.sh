#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

if $GHDL --version | grep -q "GCC back-end code"; then
    echo "GCC backend"
    set -x
    is_gcc=true
else
    is_gcc=false
fi

if [ "$is_gcc" = true ]; then
    GHDL_FLAGS="-fprofile-arcs -ftest-coverage -Wl,--coverage"
fi

analyze full_adder.vhdl
analyze full_adder_tb.vhdl

# Do not try to elaborate, libgcov may not be available

if [ "$is_gcc" = true ]; then
    ls -l
    test -f full_adder.gcno
    test -f full_adder_tb.gcno
fi

clean

echo "Test successful"
