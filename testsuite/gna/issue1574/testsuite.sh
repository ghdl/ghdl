#! /bin/sh

. ../../testenv.sh

# Breaking the pure rule is only a warning with -frelaxed, so analysis
# carries on with a function declared pure that is not.  What GHDL records
# about that must not change what the source said: Pure_Flag is what the
# source declared -- it is what reprint prints, and what the other pure
# rules are checked against -- so the analysed purity is kept apart from it.
# See issue1574, whose synthesis half is in testsuite/synth/issue1574.
export GHDL_STD_FLAGS="--std=08 -frelaxed"

out=$(analyze pure.vhdl 2>&1)
echo "$out"

#  Every violation must be reported, not just the first one: two signal
#  references and one file declaration.
n=$(echo "$out" | grep -c "violate pure rule")
if [ "$n" -ne 2 ]; then
  echo "FAIL: expected 2 pure rule warnings, got $n"
  exit 1
fi

if ! echo "$out" | grep -q "cannot declare a file in a pure function"; then
  echo "FAIL: the file declaration rule stopped being checked"
  exit 1
fi

#  And the reprinted source must still say "pure function".
"$GHDL" --reprint $GHDL_STD_FLAGS $GHDL_FLAGS pure.vhdl > reprint.vhdl 2> /dev/null

if grep -q "impure function f" reprint.vhdl; then
  echo "FAIL: reprint turned the pure function into an impure one"
  cat reprint.vhdl
  exit 1
fi

if ! grep -q "pure function f" reprint.vhdl; then
  echo "FAIL: reprint lost the function"
  cat reprint.vhdl
  exit 1
fi

rm -f reprint.vhdl

#  ghdl fmt reads the source back and prints it from the AST, and it stops
#  with an internal error when the two disagree ("need to print: impure,
#  but read pure from file"), which is what an editor integration runs.
"$GHDL" fmt $GHDL_STD_FLAGS $GHDL_FLAGS pure.vhdl > fmt.vhdl 2> fmt.err
if [ $? -ne 0 ] || grep -q "GHDL Bug occurred" fmt.err; then
  echo "FAIL: ghdl fmt no longer accepts the file"
  cat fmt.err
  exit 1
fi
rm -f fmt.vhdl fmt.err

#  A "process (all)" that calls such a function must be sensitive to the
#  signal the function reads: that comes from Sem_Call_All_Sensitized_Check,
#  which also has to look at the analysed purity rather than at Pure_Flag.
analyze allsens.vhdl

out=$(elab_simulate allsens 2>&1)
echo "$out"

if ! echo "$out" | grep -q "o = ''1''"; then
  echo "FAIL: the all-sensitized process is not sensitive to the signal"
  exit 1
fi

clean

echo "Test successful"
