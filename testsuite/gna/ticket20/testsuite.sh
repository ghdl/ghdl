#!/bin/sh

. ../../testenv.sh

analyze morten1.vhdl
elab_simulate morten

clean