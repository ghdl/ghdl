#! /bin/sh

. ../../testenv.sh

analyze phonybench.vhdl
elab phonybench
if ghdl_has_feature phonybench fst; then
  elab_simulate phonybench --stop-time=1sec --fst=pb.fst
fi
if ghdl_has_feature phonybench vcd; then
  elab_simulate phonybench --stop-time=1sec --vcd=pb.vcd
fi

rm -f pb.fst pb.vcd

clean

echo "Test successful"
