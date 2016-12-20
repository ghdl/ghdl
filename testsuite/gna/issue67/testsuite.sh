#! /bin/sh

# Missing SEH handler on windows-64.
if [ "$OS" = "Windows_NT" -a "$PROCESSOR_ARCHITECTURE" = "AMD64" ]; then
  echo "Test skipped"
  exit 0
fi

. ../../testenv.sh

analyze nullacc.vhdl
elab_simulate_failure nullacc

analyze fileerr.vhdl
elab_simulate_failure fileerr

clean

echo "Test successful"
