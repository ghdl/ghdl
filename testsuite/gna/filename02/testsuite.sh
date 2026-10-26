#! /bin/sh

. ../../testenv.sh

cp repro.vhdl 'source$(shell touch GHDL_MAKE_POC).vhdl'
analyze 'source$(shell touch GHDL_MAKE_POC).vhdl'

if "$GHDL" --gen-makefile makepoc > Makefile.generated 2> makefile.err; then
  echo "Makefile generation unexpectedly accepted an unsafe filename"
  exit 1
fi

test ! -s Makefile.generated
test ! -e GHDL_MAKE_POC
grep -F -q 'cannot generate Makefile: unsafe filename' makefile.err

rm -f -- 'source$(shell touch GHDL_MAKE_POC).vhdl' Makefile.generated makefile.err
clean

echo "Test successful"
