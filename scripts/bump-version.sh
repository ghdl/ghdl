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

# Version should be:
case "$ver"@ in
    [0-9].[0-9].[0-9]@) echo "Release version $ver";;
    [0-9].[0-9].[0-9]-dev@) echo "Development branch $ver";;
    [0-9].[0-9].[0-9]-rc[0-9]@) echo "Release candidate $ver";;
    *) echo "Incorrect version name ($ver)"; exit 3;;
esac

set -e

sed -i "s/^ghdl_version=.*/ghdl_version=\"$ver\"/" configure
sed -i "s/^__version__ =.*/__version__ = \"$ver\"/" pyGHDL/__init__.py

echo "Next steps:"
echo "git add -p"
echo "git commit -m \"Bump version to $ver\""
echo "git tag v$ver"
echo "git push github release-vX"
echo "git push github v$ver"

exit 0
