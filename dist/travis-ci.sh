PKG_VER=`grep Ghdl_Ver src/version.in | sed -e 's/.*"\(.*\)";/\1/'`

if [ "$TRAVIS_TAG" = "" ]; then
    PKG_TAG=`date -u +%Y%m%d`
else
    PKG_TAG="$TRAVIS_TAG"
fi

export PKG_FILE="ghdl-$PKG_VER-$BLD-$PKG_TAG.tgz"
