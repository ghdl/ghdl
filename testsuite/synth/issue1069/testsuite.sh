#! /bin/sh

. ../../testenv.sh

for f in tdp_ram tdp_ram2 ram3 ram4 ram5; do
    synth_tb $f 2>&1 | tee $f.log
    grep "found RAM" $f.log
done

#synth_tb ram41

clean

echo "Test successful"
