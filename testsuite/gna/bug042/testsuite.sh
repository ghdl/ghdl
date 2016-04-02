#! /bin/sh

. ../../testenv.sh

analyze_failure centerconfig.vhdl
analyze centerconfig_works.vhdl
elab_simulate instance

clean

echo "Test successful"
