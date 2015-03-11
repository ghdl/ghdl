#! /bin/sh

. ../../testenv.sh

analyze phonybench.vhdl
elab_simulate phonybench --stop-time=1sec --fst=pb.fst

rm -f pb.fst pb.ghw

clean

echo "Test successful"
