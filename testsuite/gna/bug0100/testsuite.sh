#! /bin/sh

. ../../testenv.sh

analyze_failure --force-analysis inst1.vhdl
analyze_failure --force-analysis notype1.vhdl

if analyze_failure --force-analysis notype1.vhdl 2>&1 | grep -q "indexed name"; then
  :
else
  echo "FAIL: missing error message from semantic analysis"
fi

clean

echo "Test successful"
