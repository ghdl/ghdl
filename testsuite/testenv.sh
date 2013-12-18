# Testsuite environment
#
# This file defines the shell functions to analyse a file, elaborat or run
# a design. There are version for expected success and expected failure.
#
# Every test should source and use this file.

# Note: the executable must be set in the GHDL environment variable

# User defined variables (can be set to run the testsuite in some
#  configuration, such as optimization or debugging):
# GHDL_FLAGS
# GHDL_ELABFLAGS
# GHDL_SIMFLAGS

#GHDL=ghdl
RM=rm
LN=ln

# Exit in case of failure in shell scripts.
set -e

# Analyze files (no error expected)
analyze ()
{
   echo "analyze $@"
   $GHDL -a $GHDL_FLAGS $@
}

# Analyze files (failure expected)
analyze_failure ()
{
   echo "try to analyze $@"
   # for arg in $@; do echo "arg: $arg"; done
   if ! $GHDL -a --expect-failure $GHDL_FLAGS $@ ; then
     echo "Failure expected"
     return 1
   fi
}

# Elaborate a design (no error expected)
# Note: somewhat deprecated, use elab_simulate instead.
elab ()
{
   echo "elaborate $@"
   $GHDL -e $GHDL_FLAGS $GHDL_ELABFLAGS $@
}

# Elaborate a design (failure expected)
# Note: somewhat deprecated, use elab_simulate_failure instead.
elab_failure ()
{
   echo "elaborate (failure expected) $@"
   $GHDL -e --expect-failure $GHDL_FLAGS $GHDL_ELABFLAGS $@
}

# Simulate a design (no error expected)
# Note: somewhat deprecated, use elab_simulate instead.
simulate ()
{
   echo "simulate $@ ($GHDL_FLAGS $@ $GHDL_SIMFLAGS)" >&2
   $GHDL -r $GHDL_FLAGS $@ $GHDL_SIMFLAGS
   #./$@
}

# Simulate a design (failure expected)
# Note: somewhat deprecated, use elab_simulate_failure instead.
simulate_failure ()
{
   echo "simulate (failure expected) $@" >&2
   $GHDL -r $GHDL_FLAGS $@ --expect-failure
   #./$@
}

# Elaborate and simulate a design (no error expected)
elab_simulate ()
{
   echo "elaborate and simulate $@"
   $GHDL --elab-run $GHDL_FLAGS $GHDL_ELABFLAGS $@
}

# Elaborate and simulate a design (failure expected)
elab_simulate_failure ()
{
   echo "elaborate and simulate (failure expected) $@"
   $GHDL --elab-run $GHDL_FLAGS $GHDL_ELABFLAGS $@ --expect-failure
}

# Run a program
run ()
{
   echo "run $@"
   eval $@
}

# Run a program (failure expected)
run_failure ()
{
   echo "run (failure expected) $@"
   if eval $@; then
     echo "failure expected";
     false;
   fi
}

# Clean the environment
clean ()
{
  if [ $# -eq 0 ]; then
    echo "Remove work library"
    $GHDL --remove
  else
    echo "Remove $1 library"
    $GHDL --remove --work=$1
  fi
}
