#! /bin/sh

. ../../testenv.sh

analyze fail.vhd
elab_simulate mat_key_tb

clean

echo "Test successful"
