#! /bin/sh

. ../../testenv.sh

# The reduction posted on the issue: an unconstrained formal port whose
# actual goes through a conversion function that widens it.
analyze repro.vhdl
elab_simulate test

clean

# Same shape, but checking the value seen by the formal and not only that
# elaboration survives.
analyze conv.vhdl
elab_simulate conv

clean

echo "Test successful"
