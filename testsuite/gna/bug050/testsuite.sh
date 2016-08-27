#! /bin/sh

. ../../testenv.sh

analyze_failure tarith.vhdl
analyze --ieee=synopsys tarith.vhdl

analyze_failure tunsigned.vhdl
analyze --ieee=synopsys  tunsigned.vhdl

analyze_failure tsigned.vhdl
analyze --ieee=synopsys  tsigned.vhdl

analyze_failure tsltextio.vhdl
analyze --ieee=synopsys  tsltextio.vhdl
analyze --std=08 tsltextio.vhdl

clean

clean --std=08

echo "Test successful"
