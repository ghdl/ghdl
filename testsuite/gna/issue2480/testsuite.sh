#! /bin/sh

. ../../testenv.sh

analyze foo.vhdl
elab foo
if ghdl_has_feature foo vcd; then
  simulate foo --vcd=foo.vcd --vcd-enums --vcd-nodate

  diff_nocr foo.ref foo.vcd
fi

clean
rm -f foo.vcd

echo "Test successful"
