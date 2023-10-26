#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if ! ghdl_is_preelaboration; then
    exit 0
fi

files="hier1 hier2 type1 enum1 enum2 forgen1 casegen1
 ifgen1 compinst1 compinst2"

for f in $files; do
    analyze ${f}.vhdl
    elab_simulate $f --fst=${f}.fst --fst-nodate
    if [ -f refs/${f}.fst ]; then
	# Test only if available (fst files are not very deterministic)
	cmp ${f}.fst refs/${f}.fst
    fi
done

clean

echo "Test successful"
