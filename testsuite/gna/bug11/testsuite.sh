#! /bin/sh

. ../../testenv.sh

analyze phonybench.vhdl
elab_simulate phonybench --fst=pb.fst

#clean

echo "Test successful"
