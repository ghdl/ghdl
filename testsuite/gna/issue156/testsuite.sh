#! /bin/sh

. ../../testenv.sh

analyze compile_error.vhdl
analyze repro1.vhdl
clean

echo "Test successful"
