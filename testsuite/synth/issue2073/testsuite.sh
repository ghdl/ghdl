#! /bin/sh

. ../../testenv.sh

synth_only ivoice

synth_tb ivoice2

clean

echo "Test successful"
