#! /bin/sh

. ../../testenv.sh

analyze etapa.vhdl cordic.vhdl desenrollado_tb.vhdl 
analyze cordic.vhdl desenrollado_tb.vhdl etapa.vhdl 

clean

echo "Test successful"
