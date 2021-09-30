#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab repro
if ghdl_has_feature repro vcd; then
  simulate repro --vcd=repro-std.vcd
  simulate repro --vcd=repro-vlg.vcd --vcd-4states

  if fgrep -q "U!" repro-vlg.vcd; then
    echo "error: non-verilog state in vcd"
    exit 1;
  fi
fi

clean
rm -f repro-*.vcd

echo "Test successful"
