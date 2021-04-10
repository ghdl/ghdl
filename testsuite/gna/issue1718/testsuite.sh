#! /bin/sh

. ../../testenv.sh

analyze --work=my_lib ent.vhdl
elab_simulate my_lib.ent

! elab_simulate .ent
! elab_simulate my_lib.

clean my_lib

echo "Test successful"
