#! /bin/sh

. ../../testenv.sh

analyze -P. hello.vhdl
analyze -P=. hello.vhdl

# Cannot use analyze_failure for errors in options.
if analyze -P= hello.vhdl; then
  echo "failure expected for -P="
  exit 1;
fi
if analyze -P hello.vhdl; then
  echo "failure expected for -P"
  exit 1;
fi

clean

echo "Test successful"
