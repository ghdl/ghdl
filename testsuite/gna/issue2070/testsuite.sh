#! /bin/sh

. ../../testenv.sh

files="
crash0.vhdl
crash1.vhdl
crash10.vhdl
crash11.vhdl
crash12.vhdl
crash13.vhdl
crash14.vhdl
crash15.vhdl
crash16.vhdl
crash17.vhdl
crash18.vhdl
crash19.vhdl
crash2.vhdl
crash20.vhdl
crash21.vhdl
crash22.vhdl
crash23.vhdl
crash24.vhdl
crash25.vhdl
crash26.vhdl
crash27.vhdl
crash28.vhdl
crash29.vhdl
crash3.vhdl
crash30.vhdl
crash31.vhdl
crash32.vhdl
crash33.vhdl
crash34.vhdl
crash35.vhdl
crash36.vhdl
crash37.vhdl
crash38.vhdl
crash39.vhdl
crash4.vhdl
crash40.vhdl
crash41.vhdl
crash42.vhdl
crash43.vhdl
crash44.vhdl
crash45.vhdl
crash46.vhdl
crash47.vhdl
crash48.vhdl
crash49.vhdl
crash5.vhdl
crash50.vhdl
crash51.vhdl
crash52.vhdl
crash53.vhdl
crash54.vhdl
crash55.vhdl
crash6.vhdl
crash7.vhdl
crash8.vhdl
crash9.vhdl
"

export GHDL_STD_FLAGS=--std=08
for f in $files; do
    analyze_failure $f
done

clean

echo "Test successful"
