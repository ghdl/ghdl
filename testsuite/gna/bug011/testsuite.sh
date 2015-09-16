#! /bin/sh

. ../../testenv.sh

analyze phonybench.vhdl
elab phonybench
if ghdl_has_feature phonybench fst; then
  elab_simulate phonybench --stop-time=1sec --fst=pb.fst
fi

rm -f pb.fst pb.ghw

clean

echo "Test successful"
