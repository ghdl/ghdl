#! /bin/sh

. ../../testenv.sh

# Expect only one error.
analyze_failure repro1.vhdl 2> repro1.err
if ! diff_nocr repro1.err repro1.ref; then
  echo "unexpected output"
  exit 1;
fi

rm -f repro1.err
clean

echo "Test successful"
