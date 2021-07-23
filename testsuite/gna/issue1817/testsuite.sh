#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

if $GHDL --version | grep -q "GCC back-end code"; then
    is_gcc=true
else
    is_gcc=false
fi

if [ "$is_gcc" = true ]; then
    GHDL_FLAGS="-fprofile-arcs -ftest-coverage -Wl,--coverage"
    rm -f *.gcno
fi

analyze full_adder.vhdl
analyze full_adder_tb.vhdl

# Do not try to elaborate, libgcov may not be available

if [ "$is_gcc" = true ]; then
    # The name of the gcno file is sometimes .gcno, sometimes .vhdl.gcno
    # It is built on the -auxbase NAME option, which is not given by ghdl.
    # The default is to remove the extension from the source file, but at
    # most 4 characters are removed.
    test -f full_adder.gcno
    test -f full_adder_tb.gcno
fi

clean

echo "Test successful"
