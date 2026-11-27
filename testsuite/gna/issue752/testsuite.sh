#! /bin/sh

. ../../testenv.sh

# The line tgingold identified on the thread, from VUnit's string_ptr_pkg:
#
#   new_ptr := new string'(1 to length => character'low);
#
# The aggregate is built in a temporary before being copied into the
# allocated object, and that temporary is on the stack.  With a large LENGTH
# the stack overflows, and GHDL said nothing at all -- SIGSEGV, core dumped.
#
# __ghdl_check_stack_allocation and --max-stack-alloc already existed, but
# were only generated for the declaration of a complex object, not for this
# temporary.  Now the object too large is reported.

export GHDL_STD_FLAGS=--std=08

analyze repro.vhd

if out=$(elab_simulate repro 2>&1); then
  echo "$out"
  echo "FAIL: expected the oversized stack allocation to be reported"
  exit 1
fi
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: internal error instead of a diagnostic"
  exit 1
fi

if ! echo "$out" | grep -q "declaration of a too large object"; then
  echo "FAIL: expected the too-large-object diagnostic"
  exit 1
fi

# Without the limit it must still work, given enough stack.
elab_simulate repro --max-stack-alloc=0 -gLENGTH=1024

clean

echo "Test successful"
