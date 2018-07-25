#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab repro
if ghdl_has_feature repro vcd; then
  simulate repro --vcd=repro.vcd

  if ! grep -q upscope repro.vcd; then
    echo "missing scope in vcd"
    exit 1;
  fi
fi

clean
rm -f repro.vcd

echo "Test successful"
