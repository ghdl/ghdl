#! /bin/sh

. ../../testenv.sh

# `unaffected` as the else-branch of a conditional signal assignment
# gated by rising_edge() used to crash --synth with an internal
# TYPES.INTERNAL_ERROR (synth-stmts.adb:75). See issue1504.
synth test1.vhd -e ent > syn_test1.vhd
analyze syn_test1.vhd

clean

echo "Test successful"
