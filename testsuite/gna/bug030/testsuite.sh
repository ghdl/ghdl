#! /bin/sh

. ../../testenv.sh

analyze --warn-reserved reserved1.vhdl
analyze --warn-reserved reserved2.vhdl
analyze --warn-reserved reserved3.vhdl
analyze --warn-reserved --std=87 reserved4.vhdl

clean
clean --std=87

echo "Test successful"
