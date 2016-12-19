#! /bin/sh

# Missing SEH handler on windows-64.
echo "Test skipped"
exit 0

. ../../testenv.sh

analyze nullacc.vhdl
elab_simulate_failure nullacc

analyze fileerr.vhdl
elab_simulate_failure fileerr

clean

echo "Test successful"
