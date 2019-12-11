#!/usr/bin/env sh

cd "$(dirname $0)"

set -e

ghdl -a -O0 -g tb.vhd
ghdl -e -O0 -g -Wl,-I./ -Wl,main.c tb
./tb
