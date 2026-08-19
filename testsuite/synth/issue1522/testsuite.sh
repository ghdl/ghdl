#! /bin/sh

. ../../testenv.sh

# Indexing an array inside a record (with several array members) via a
# to_integer() conversion used to crash --synth with an internal
# TYPES.INTERNAL_ERROR (netlists-memories.adb:336) instead of inferring
# a ROM lookup. See issue1522.
synth ent.vhd -e ent > syn_ent.vhd
analyze syn_ent.vhd

clean

echo "Test successful"
