#! /bin/sh

cd $(dirname "$0")

. ../../testenv.sh

if $GHDL --help | grep -q -e --link; then
    if [ -z "$CC" ]; then
        CC="gcc"
    fi

    analyze hello.vhdl
    elab -Wl,main.c hello
    run ./hello

    clean
    rm -f main.o

    analyze hello.vhdl
    elab -Wl,main_notnull.c hello
    run ./hello

    clean
    rm -f main.o
else
    echo "Test not supported by the configuration"
fi

echo "Test successful"
