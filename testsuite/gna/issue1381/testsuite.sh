#! /bin/sh

. ../../testenv.sh

analyze -Wunused -Werror repro.vhdl
analyze_failure repro2.vhdl
analyze_failure repro3.vhdl
analyze -Wunused -Werror repro4.vhdl

# Extra tests about -Wunused
analyze_failure -Wunused -Werror repro5.vhdl
analyze_failure -Wunused -Werror repro6.vhdl

clean

echo "Test successful"
