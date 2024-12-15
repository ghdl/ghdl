#! /bin/sh

. ../../testenv.sh

set -x

if c_compiler_is_available; then
	lib=$("$GHDL" --libghdl-name)
	lib_path="$("$GHDL" --libghdl-library-path)"
	if [ ! -f "$lib_path" ]; then
	    echo "no libghdl"
	    exit 0
	fi
	lib=${lib#lib} # strip "lib" prefix
	lib=${lib%.*} # strip .so/.dll suffix

	libdir="$(dirname "$lib_path")"
	incdir="$("$GHDL" --libghdl-include-dir)"

	$CC -I"$incdir" test.c "$("$GHDL" --libghdl-library-path)"
	$CC -I"$incdir" -L"$libdir" test.c -l"$lib"
	rm -f a.out
fi

echo "Test successful"
