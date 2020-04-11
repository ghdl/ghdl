#! /bin/sh

. ../../testenv.sh

if $GHDL --help | grep -q -e --link; then
    if [ -z $CC ]; then
	CC="gcc"
    fi

    $CC -c main.c
    analyze hello.vhdl
    elab -Wl,main.o hello
    run ./hello

    clean
    rm -f main.o
else
    echo "Test not supported by the configuration"
fi

echo "Test successful"
