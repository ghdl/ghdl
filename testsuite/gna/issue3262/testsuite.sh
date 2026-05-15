#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure pack.vhdl
analyze_failure --force-analysis pack.vhdl
analyze_failure --force-analysis pack2.vhdl

analyze --work=mylib core1.vhdl

clean
echo "Test successful"
