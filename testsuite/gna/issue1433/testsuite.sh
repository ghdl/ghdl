#! /bin/sh

. ../../testenv.sh

# An entity that recursively self-instantiates (via "entity
# library.name" direct instantiation) with swapped generic-specified
# port widths used to fail with a spurious runtime bound-check error
# instead of correctly swapping and re-elaborating. See issue1433.
analyze example_recursive_instantiation_entity_style.vhd

out=$(elab_simulate example_recursive_instantiation_entity_style -gA_WIDTH=2 -gB_WIDTH=3 2>&1)
echo "$out"

if ! echo "$out" | grep -q "A_WIDTH=2 < B_WIDTH=3, so swap them."; then
  echo "FAIL: expected the swap-detection message"
  exit 1
fi

if ! echo "$out" | grep -q "Do stuff with A_WIDTH=3 >= B_WIDTH=2\.\.\."; then
  echo "FAIL: expected the recursive instance to run with swapped generics"
  exit 1
fi

clean

echo "Test successful"
