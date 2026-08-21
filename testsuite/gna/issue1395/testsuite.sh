#! /bin/sh

. ../../testenv.sh

# STRING is "array (POSITIVE range <>) of CHARACTER", so an index constraint
# that includes 0 is invalid.  The gcc and llvm backends did not check the
# bounds of a new index constraint against the index subtype of the array
# being constrained, and accepted e.g. string(4 downto 0) whenever a bound
# was not locally static.  See issue1395.

# Bounds coming from a generic, on a variable and on a signal.
analyze dyn_zero.vhdl
elab_simulate_failure dyn_zero
clean

analyze dyn_zero_signal.vhdl
elab_simulate_failure dyn_zero_signal
clean

# The reproducer from the issue itself: the bounds come from the attributes
# of an unconstrained parameter, and are only known on the call.
analyze func_attr.vhdl
elab_simulate_failure func_attr
clean

# Not a STRING: any array whose index subtype excludes the value.
analyze user_array.vhdl
elab_simulate_failure user_array
clean

# The constraint of an allocator goes through the same code.
analyze alloc.vhdl
elab_simulate_failure alloc
clean

# And the other direction: valid constraints must still elaborate.  A null
# range is compatible with any index subtype whatever its bounds.
analyze ok_ranges.vhdl
elab_simulate ok_ranges
clean

echo "Test successful"
