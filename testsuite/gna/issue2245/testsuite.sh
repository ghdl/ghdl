#! /bin/sh

. ../../testenv.sh

# LRM08 16.2 allows T'BASE only as the prefix of the name of another
# attribute -- T'BASE'LEFT is its own example -- and never as a type mark.
#
# When T was an array type, GHDL resolved such a prefix with Sem_Type_Mark,
# which wrongly rejected it ("'Base attribute cannot be used as a type
# mark"); finishing the 'ELEMENT name then crashed with an internal
# ASSERT_FAILURE ("no field Name_Staticness"), Iir_Kind_Base_Attribute
# having no such field.  test.vhdl is the report's repro.
#
# pos.vhdl covers the neighbouring legal forms, including T'BASE'ELEMENT as
# a type mark, which crashed the mcode elaborator in exec_name_subtype once
# it started analyzing.
#
# err.vhdl covers what must stay rejected -- and rejected cleanly: 'BASE in
# a real type mark position used to be reported and then carried on, and
# crashed in get_type_of_subtype_indication.
# See issue2245.
export GHDL_STD_FLAGS="--std=08"

# --------------------------------------------------------------------------
# The design from the report.
# --------------------------------------------------------------------------
analyze test.vhdl

out=$(elab_simulate unc_tbase3 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: analysis or simulation of test.vhdl crashed"
  exit 1
fi

for val in -2147483648 22 23; do
  if ! echo "$out" | grep -q "(report note): $val"; then
    echo "FAIL: test.vhdl did not report $val"
    exit 1
  fi
done

# --------------------------------------------------------------------------
# The other legal uses of 'BASE.
# --------------------------------------------------------------------------
analyze pos.vhdl

out=$(elab_simulate unc_tbase3_pos 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: analysis or simulation of pos.vhdl crashed"
  exit 1
fi

#      2147483647  integer'base'high            (scalar T'BASE, the LRM example)
#      11          ints8'element'succ(10)       (control, no 'BASE)
#      21          ints8'base'element'succ(20)
#      31          mat2'base'element'element'succ(30)
#      41          integer'base'base'succ(40)   (chained 'BASE)
#      55          a variable of type ints8'base'element
for val in 2147483647 11 21 31 41 55; do
  if ! echo "$out" | grep -q "(report note): $val"; then
    echo "FAIL: pos.vhdl did not report $val"
    exit 1
  fi
done

# --------------------------------------------------------------------------
# What must stay rejected, without an internal error.
# --------------------------------------------------------------------------
out=$(analyze_failure err.vhdl 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: analysis of err.vhdl crashed instead of reporting clean errors"
  exit 1
fi

check_err()
{
  if ! echo "$out" | grep -q "$1"; then
    echo "FAIL: err.vhdl did not report: $1"
    exit 1
  fi
}

#  signal s : ints8'base;
check_err "'Base attribute cannot be used as a type mark"
#  ints8'base'left -- legal 'BASE, but an unconstrained base type
check_err "prefix type is not constrained"
#  sig'base'... -- the prefix of 'BASE must denote a type
check_err "a type mark must denote a type or a subtype"
#  integer'base alone
check_err "'base attribute not allowed in an expression"

clean

echo "Test successful"
