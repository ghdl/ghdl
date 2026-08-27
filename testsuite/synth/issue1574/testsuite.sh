#! /bin/sh

. ../../testenv.sh

# A function that breaks the pure rule and is called from a clocked process
# crashed --synth with an internal assertion failure: synthesis keeps the
# instance const for a function whose Pure_Flag is set, which selects the
# static call path, which then asserts that the result is static.  It is not,
# because the function reads a signal.
#
# The pure rule is only relaxed here -- a warning, since --synth turns
# -frelaxed on -- so analysis carries on, and the four designs below are the
# four ways the AST ends up with a function that is not pure:
#   bug.vhdl        the function is declared impure (the report's design)
#   bug_pure.vhdl   declared pure (implicitly), reads a signal directly
#                   ("Still crashes if it's marked as pure instead of impure")
#   pure_call.vhdl  declared pure, calls an impure function
#   pure_proc.vhdl  declared pure, calls a procedure that reads a signal
#   pure_chain.vhdl declared pure, calls a function that is declared pure
#                   and reads a signal -- the impurity has to propagate
#   pure_via_proc.vhdl  a procedure calls such a function, so the procedure
#                   is impure too and the process calling it must not be
#                   const-folded either
# See issue1574.
for d in bug bug_pure pure_call pure_proc pure_chain pure_via_proc; do
  synth $d.vhdl -e $d > syn_$d.vhdl
  analyze syn_$d.vhdl
done

clean

echo "Test successful"
