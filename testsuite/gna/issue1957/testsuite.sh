#! /bin/sh

. ../../testenv.sh

# A record whose elements are unconstrained (std_logic_vector), used as a
# constrained entity port type -- t_config(param_a(31 downto 0), ...) --
# hit a stale assertion in Translate_Selected_Element: it required the
# type info of the field to equal that of the element declaration, while
# the function's own comment says the two differ precisely when the record
# subtype constrains the element.  Only the gcc and llvm backends use that
# translator.
# chk.vhd checks the values that arrive through the port, not just that
# the design analyzes.  See issue1957.
export GHDL_STD_FLAGS=--std=08

analyze test_pkg.vhd
analyze dut.vhd
analyze tb_dut.vhd
analyze chk.vhd

out=$(elab_simulate tb_chk 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed instead of elaborating"
  exit 1
fi

if ! echo "$out" | grep -q "PASS param_a=34 param_b=55"; then
  echo "FAIL: wrong record element values through the port"
  exit 1
fi

clean

echo "Test successful"
