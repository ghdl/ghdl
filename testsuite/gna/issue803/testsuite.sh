#! /bin/sh

. ../../testenv.sh


if $GHDL --version | grep -q "GCC back-end"; then
    echo "GCC backend detected"
elif $GHDL --version | grep -q "LLVM back-end"; then
    echo "LLVM backend detected"
else
    echo "This test requires GCC or LLVM backends"
    exit 0
fi

if [ "$OS" = "Windows_NT" ]; then
    gcc main-win.c -o main
    analyze tb.vhdl
    $GHDL -e -Wl,-shared -Wl,-Wl,-u,ghdl_main -o tb.dll tb
    ./main
    rm main.exe tb.dll ./*.o
elif [ "$(uname -o)" = "Darwin" ]; then
    gcc main-mac.c -o main
    analyze tb.vhdl
    $GHDL -e -Wl,-shared -Wl,-Wl,-u,_ghdl_main -o tb.dylib tb
    ./main
    rm main tb.dylib ./*.o
else
    echo "This test is disabled on Linux (requires an -fpic libgrt)"
    exit 0
fi

clean

echo "Test successful"
