#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze pkg.vhdl
elab pkgtop

#  Exercise packages[] with constants, architecture constants, and the
#  type constraint suffix (rendered from folded static expressions).
simulate pkgtop --flow=pkg.flow --stop-time=0ns

if test ! -s pkg.flow; then
  echo "FAILED: --flow did not create pkg.flow"
  exit 1
fi

check ()
{
  if ! grep -qF "$1" pkg.flow; then
    echo "FAILED: expected $2 in pkg.flow"
    exit 1
  fi
}

#  Package constants with resolved values.
check '"name": "width", "type": "natural", "value": 16'  "package constant width"
check '"name": "depth", "type": "natural", "value": 256' "package constant depth"

#  Architecture constant, folded from WIDTH + 1.
check '"name": "local", "type": "natural", "value": 17'  "folded local constant"

#  Type constraint suffix, folded from WIDTH - 1 downto 0.
check '"type": "std_logic_vector(15 downto 0)"'          "type constraint suffix"

clean
rm -f pkg.flow

echo "Test successful"
