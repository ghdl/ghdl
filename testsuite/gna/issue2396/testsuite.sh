#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

# Analyze and check hide warning number.
analyze -Whide repro_warn1.vhdl 2>&1 | grep -c warning > warning.cnt
if [ $(cat warning.cnt) -ne 1 ]; then
    echo "incorrect warning number"
    exit 1
fi
rm warning.cnt

analyze frequency.vhdl
analyze tb_freq.vhdl
elab_simulate tb_freq --stop-time=1us

clean

echo "Test successful"
