#! /bin/sh

. ../../testenv.sh

analyze_failure centerconfig.vhdl
analyze centerconfig_works.vhdl
elab_simulate instance

analyze_failure centerconfig_generics_works.vhdl

clean

echo "Test successful"
