#! /bin/sh

. ../../testenv.sh

analyze ax_wb_pli_pkg.vhdl
analyze ax_wb_pli.vhdl

# Don't try to compile the C file, it's unix specific

clean

echo "Test successful"
