#! /bin/sh

. ../../testenv.sh

analyze testit.vhdl
elab_simulate testit

analyze repro.vhdl
elab_simulate repro

analyze bit_vector_rol_ror.vhdl
elab_simulate bit_vector_rol_ror

clean

echo "Test successful"
