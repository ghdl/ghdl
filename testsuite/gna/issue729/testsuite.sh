#! /bin/sh

. ../../testenv.sh

analyze -Werror ent.vhdl
analyze_failure -Werror -Wothers ent.vhdl

analyze_failure -Werror whide.vhdl
analyze -Werror -Wno-hide whide.vhdl
clean

echo "Test successful"
