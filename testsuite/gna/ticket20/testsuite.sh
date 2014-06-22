#!/bin/sh

. ../../testenv.sh

analyze morten1.vhdl
elab_simulate morten

analyze_failure morten2.vhdl

clean
