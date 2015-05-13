#! /bin/sh

. ../../testenv.sh

# Build from scratch
cp ref.vhdl bug.vhdl
analyze bug.vhdl
elab_simulate ent

# No change.
touch bug.vhdl
elab_simulate ent

# Still no change.
cp ref.vhdl bug.vhdl
elab_simulate ent

# Modify file
cp ref1.vhdl bug.vhdl
if elab_simulate ent; then
  # cannot use elab_simulate_failure as it doesn't work in that case (it's a
  # feature: it expects only simulation failures).
  echo "failure expected"
  exit 1
fi

# Re-analyze
analyze bug.vhdl
elab_simulate ent

rm -f bug.vhdl
clean

echo "Test successful"
