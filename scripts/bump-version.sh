#!/bin/sh

if [ $0 != "./scripts/bump-version.sh" ]; then
    echo "$0 must be called from ghdl base directory"
    exit 2
fi

if [ $# -ne 1 ]; then
    echo "usage: $0 VERSION"
    exit 2
fi

ver=$1

set -e

sed -i "s/^ghdl_version=.*/ghdl_version=\"$ver\"/" configure
sed -i "s/^__version__ =.*/__version__ = \"$ver\"/" pyGHDL/__init__.py

for f in dist/msys2/*/PKGBUILD; do
    sed -i "s/^pkgver=.*/pkgver=$ver/" $f
done

echo "Done"
exit 0
