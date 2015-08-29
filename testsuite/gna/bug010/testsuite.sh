#! /bin/sh

. ../../testenv.sh

analyze FIFO.vhdl TestFIFO.vhdl
elab_simulate testfifo --stop-time=4us

clean

echo "Test successful"
