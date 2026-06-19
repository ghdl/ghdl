#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze comp.vhdl
elab comptop

#  Component instantiation (component declaration + default binding), as
#  opposed to direct entity instantiation: the hierarchy must resolve the
#  bound entity/architecture, and the net joining the two cells must unify
#  the formal/actual ports across the component boundary.
simulate comptop --flow=comp.flow --stop-time=0ns

if test ! -s comp.flow; then
  echo "FAILED: --flow did not create comp.flow"
  exit 1
fi

check ()
{
  if ! grep -qF "$1" comp.flow; then
    echo "FAILED: expected $2 in comp.flow"
    exit 1
  fi
}

#  Both component instantiations bound to entity leaf(rtl).
check '"instance_label": "u0"' "first component instance"
check '"instance_label": "u1"' "second component instance"
check '"module": "leaf"'       "component bound to entity"

#  The internal net t unifies the collapsed ports of both instances.
check '"name": "t", "path"'    "internal chaining net"
check '"aliases": ["y", "y", "a", "a"]' "cross-component net unification"

clean
rm -f comp.flow

echo "Test successful"
