# Testsuite environment
#
# This file defines the shell functions to analyse a file, elaborate or run
# a design. There are version for expected success and expected failure.
#
# Every test should source and use this file.

# Note: the executable must be set in the GHDL environment variable

# User defined variables (can be set to run the testsuite in some
#  configuration, such as optimization or debugging):
# GHDL_STD_FLAG
# GHDL_FLAGS
# GHDL_ELABFLAGS
# GHDL_SIMFLAGS

#GHDL=ghdl
RM=rm
LN=ln

# Exit in case of failure in shell scripts.
set -e

# Define colors
ANSI_NOCOLOR="\033[0m"
ANSI_RED="\033[31m"
ANSI_BLUE="\033[34m"
ANSI_GREEN="\033[32m"
# Optionally disable colors
if [ -z "$ENABLECOLOR" ]; then unset ANSI_NOCOLOR ANSI_RED ANSI_BLUE ANSI_GREEN; fi

if [ x"$GHDL" = x ]; then
    echo "error: GHDL environment variable is not defined"
    exit 4
fi

# Analyze files (no error expected)
analyze ()
{
   echo "analyze $@"
   "$GHDL" -a $GHDL_STD_FLAGS $GHDL_FLAGS $@
}

# Analyze files (failure expected)
analyze_failure ()
{
   echo "try to analyze $@"
   # for arg in $@; do echo "arg: $arg"; done
   if ! "$GHDL" -a --expect-failure $GHDL_STD_FLAGS $GHDL_FLAGS $@ ; then
     echo "Failure expected"
     return 1
   fi
}

# Elaborate a design (no error expected)
# Note: somewhat deprecated, use elab_simulate instead.
elab ()
{
   echo "elaborate $@"
   "$GHDL" -e $GHDL_STD_FLAGS $GHDL_FLAGS $GHDL_ELABFLAGS $@
}

# Elaborate a design (failure expected)
# Note: somewhat deprecated, use elab_simulate_failure instead.
elab_failure ()
{
   echo "elaborate (failure expected) $@"
   "$GHDL" -e --expect-failure $GHDL_STD_FLAGS $GHDL_FLAGS $GHDL_ELABFLAGS $@
}

# Simulate a design (no error expected)
# Note: somewhat deprecated, use elab_simulate instead.
simulate ()
{
   echo "simulate $@ ($GHDL_FLAGS $@ $GHDL_SIMFLAGS)" >&2
   "$GHDL" -r $GHDL_STD_FLAGS $GHDL_FLAGS "$@" $GHDL_SIMFLAGS
   #./$@
}

# Simulate a design (failure expected)
# Note: somewhat deprecated, use elab_simulate_failure instead.
simulate_failure ()
{
   echo "simulate (failure expected) $@" >&2
   "$GHDL" -r $GHDL_STD_FLAGS $GHDL_FLAGS $@ --expect-failure
   #./$@
}

# Elaborate and simulate a design (no error expected)
elab_simulate ()
{
   echo "elaborate and simulate $@"
   "$GHDL" --elab-run $GHDL_STD_FLAGS $GHDL_FLAGS $GHDL_ELABFLAGS $@
}

# Elaborate and simulate a design (failure expected)
elab_simulate_failure ()
{
   echo "elaborate and simulate (failure expected) $@"
   "$GHDL" --elab-run $GHDL_STD_FLAGS $GHDL_FLAGS $GHDL_ELABFLAGS \
     $@ --expect-failure
}

# Check if a feature is present
ghdl_has_feature ()
{
  "$GHDL" -r $GHDL_STD_FLAGS $GHDL_FLAGS $1 --has-feature=$2
}

ghdl_is_interpretation ()
{
  "$GHDL" --version | grep -q interpretation
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
    "$GHDL" --remove $GHDL_STD_FLAGS
  else
    case "$1" in
      --std=*)
    	echo "Remove work library"
    	"$GHDL" --remove $1 ;;
      *)
	echo "Remove $1 library"
    	"$GHDL" --remove $GHDL_STD_FLAGS --work=$1 ;;
    esac
  fi
}
