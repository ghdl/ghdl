#! /bin/sh

. ../../testenv.sh

set -x

# libghdl is an Ada shared library, so it needs the GNAT runtime (libgnat)
# at run time.  gnatlink normally records the path to that runtime in the
# library itself, but Makefile.in used to defeat that by passing gnatlink's
# -R on every platform; it is only needed on macOS, where the automatic
# rpath duplicates the one from LDFLAGS and ld rejects it (#2806).
# Everywhere else -R just left libghdl with no way to find libgnat unless it
# happened to sit on the default linker search path -- so libghdl was
# unusable with a GNAT installed under its own prefix, which is what
# gna/issue2005 then failed on, with a wall of undefined GNAT symbols.

lib_path="$("$GHDL" --libghdl-library-path)"
if [ ! -f "$lib_path" ]; then
    echo "no libghdl"
    exit 0
fi

# No ldd on macOS (and it is the one platform that still needs -R).
if which ldd > /dev/null 2>&1; then
    if ldd "$lib_path" > deps.txt 2>&1; then
	if grep "not found" deps.txt > /dev/null; then
	    echo "error: $lib_path cannot resolve all its dependencies:"
	    grep "not found" deps.txt
	    echo "(libghdl was probably linked without a path to the GNAT runtime)"
	    rm -f deps.txt
	    exit 1
	fi
    fi
    rm -f deps.txt
fi

echo "Test successful"
