#! /bin/sh

. ../../testenv.sh

analyze mydesign.vhdl
elab myentity

simulate myentity --wave=dump.ghw | tee mydesign.out

gcc ../../../src/grt/ghwdump.c ../../../src/grt/ghwlib.c -I../../../src/grt/ -o ghwdump

# We're just checking that ghwdump doesn't crash on a zero length signal.
./ghwdump -ths dump.ghw > dump.txt

if diff --strip-trailing-cr dump.txt golden_dump.txt; then
    echo "The ghw dump matches."
else
    echo "The ghw dump does not match what is expected."
    exit 1
fi

#rm -f mydesign.out ghwdump dump.txt dump.ghw
clean

echo "Test Success"
