#! /bin/sh

. ../../testenv.sh

analyze dummy.vhdl
elab my_entity

if simulate --max-stack-alloc=1024 my_entity 2> run.err; then
    echo "failure expected"
    exit 1
fi

grep -q "place it after" run.err

clean

echo "Test successful"
