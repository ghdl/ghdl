#! /bin/sh

. ../../testenv.sh

analyze_failure --work=clock ent.vhdl

clean

echo "Test successful"
