#! /bin/sh

. ../../testenv.sh

analyze subBlock.vhd

analyze testCaseGood.vhd
elab_simulate testcasegood

analyze testCaseCrash.vhd
elab_failure testcasecrash

clean

echo "Test successful"
