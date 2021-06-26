#! /bin/sh

. ../../testenv.sh

for t in iassoc01 iassoc02 iassoc03 iassoc04 iassoc11 iassoc12; do
    synth_tb $t pkg.vhdl
done

echo "Test successful"
