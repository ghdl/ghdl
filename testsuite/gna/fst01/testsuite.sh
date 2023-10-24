#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if ! ghdl_is_preelaboration; then
    exit 0
fi

files="hier1 hier2
 ifgen1 compinst1 compinst2"
# "type1 enum1 enum2 forgen1 casegen1"

for f in $files; do
    analyze ${f}.vhdl
    elab_simulate $f --fst=${f}.fst --fst-nodate
    cmp ${f}.fst ${f}.ref
done

clean

echo "Test successful"
