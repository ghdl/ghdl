#! /bin/sh

. ../../testenv.sh

analyze -Werror=sensitivity sens.vhdl 

clean

echo "Test successful"
