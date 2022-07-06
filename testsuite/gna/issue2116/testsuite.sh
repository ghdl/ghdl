#! /bin/sh

. ../../testenv.sh

ok_files="
func4_0.vhdl
func4_1.vhdl
"

export GHDL_STD_FLAGS=--std=08
for f in $ok_files; do
    analyze $f
done

files="
aspect01.vhdl
aspect02.vhdl
aspect03.vhdl
attr1.vhdl
attr10.vhdl
attr11.vhdl
attr12.vhdl
attr13.vhdl
attr14.vhdl
attr15.vhdl
attr16.vhdl
attr17.vhdl
attr18.vhdl
attr19.vhdl
attr2.vhdl
attr20.vhdl
attr21.vhdl
attr22.vhdl
attr23.vhdl
attr24.vhdl
attr25.vhdl
attr26.vhdl
attr3.vhdl
attr4.vhdl
attr5.vhdl
attr6.vhdl
attr7.vhdl
attr8.vhdl
attr9.vhdl
cons01.vhdl
cons02.vhdl
cons03.vhdl
err01.vhdl
eval1.vhdl
eval2.vhdl
func1.vhdl
func2.vhdl
func3.vhdl
func4.vhdl
func5.vhdl
func6.vhdl
func7.vhdl
name01.vhdl
name02.vhdl
pkg1.vhdl
pkg10.vhdl
pkg11.vhdl
pkg12.vhdl
pkg13.vhdl
pkg14.vhdl
pkg15.vhdl
pkg2.vhdl
pkg3.vhdl
pkg4.vhdl
pkg5.vhdl
pkg6.vhdl
pkg7.vhdl
pkg8.vhdl
pkg9.vhdl
psl01.vhdl
psl02.vhdl
psl03.vhdl
psl04.vhdl
sign01.vhdl
unit01.vhdl
unit02.vhdl
unit03.vhdl
"

export GHDL_STD_FLAGS=--std=08
for f in $files; do
    analyze_failure $f
done

clean

echo "Test successful"
