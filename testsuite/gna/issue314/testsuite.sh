#! /bin/sh

. ../../testenv.sh

analyze_failure t1.vhdl 2> t1.err
grep 2:9 t1.err

analyze_failure -ftabstop=4 t1.vhdl 2> t1.err
grep 2:5 t1.err

analyze_failure -fcaret-diagnostics -ftabstop=4 t1.vhdl 2> t1.err
grep "^    err;" t1.err

analyze_failure -fcaret-diagnostics t1.vhdl 2> t1.err
grep "^        err;" t1.err

if analyze -ftabstop=0 t1.vhdl; then
    echo "error expected"
    exit 1
fi

if analyze -ftabstop=140 t1.vhdl; then
    echo "error expected"
    exit 1
fi

if analyze -ftabstop=aa t1.vhdl; then
    echo "error expected"
    exit 1
fi

rm -f t1.err

clean

echo "Test successful"
