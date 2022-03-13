#! /bin/sh

. ../../testenv.sh

set -x

if c_compiler_is_available; then
	lib=$("$GHDL" --libghdl-name)
	lib=${lib#lib} # strip "lib" prefix
	lib=${lib%.so} # strip .so suffix

	$CC -I"$("$GHDL" --libghdl-include-dir)" -L"$(dirname "$("$GHDL" --libghdl-library-path)")" test.c -l$lib
	rm -f a.out
fi

echo "Test successful"
