#!/bin/sh

set -e

cmd() { echo "$*" ; $* ; }

GHDL=${GHDL:-ghdl}
cmd $GHDL -a --work=alib asrc.vhd
cmd $GHDL -a tb.vhd
cmd $GHDL -e tb
