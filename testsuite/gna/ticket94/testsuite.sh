#! /bin/sh

. ../../testenv.sh

# Original test
analyze --work=alib asrc.vhd
analyze tb.vhd
elab_simulate tb

# Reproducer - entity would be visible in the absence of the component
analyze tb1.vhd
elab_simulate tb1

# Reproducer - entity is directly visible
analyze --work=alib apkg.vhd
analyze tb2.vhd
elab_simulate tb2

# Reproducer - entity is in the same library. (93c)
analyze tb3.vhd
elab_simulate tb3

# Reproducer - entity in a different library, not visible.
analyze tb4.vhd
elab_simulate_failure tb4
elab_simulate --syn-binding tb4

clean
clean alib

# Reproducer - entity is in the same library (93)
GHDL_STD_FLAGS=--std=93

analyze --work=alib asrc.vhd
analyze --work=alib apkg.vhd
analyze tb3.vhd
elab_simulate_failure tb3

# But still works with --syn-binding
elab_simulate --syn-binding tb3

clean
clean alib

echo "Test successful"
