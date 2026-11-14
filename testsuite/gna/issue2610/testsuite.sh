#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

for t in repro1 repro2 repro3 repro4 repro5 repro6; do
  analyze $t.vhdl
  elab_simulate $t
done

#  Errors at elaboration.
analyze err.vhdl
for t in e1 e2 e3 e4 e5 e6 e7; do
  elab_simulate_failure $t
done

if $GHDL --version | grep -q "GCC .* code generator"; then
  #  gcov checks that the elaboration subprograms of external names don't end
  #  before they start.  Only -ftest-coverage: no need for libgcov to link.
  GHDL_FLAGS=-ftest-coverage
  analyze repro1.vhdl repro5.vhdl
  elab_simulate repro5
  rm -f *.gcno
fi

clean

echo "Test successful"
