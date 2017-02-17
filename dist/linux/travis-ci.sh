IFS='+' read -ra REFS <<< "$BLD"

DBLD=${REFS[1]}
PKG_DTAG=${REFS[0]}
PKG_SHORTCOMMIT="$(echo $TRAVIS_COMMIT | cut -c1-10)"
PKG_VER=`grep Ghdl_Ver src/version.in | sed -e 's/.*"\(.*\)";/\1/'`
PKG_TAG="$TRAVIS_TAG"

if [ -z "$BUILDTHIS" ]; then BUILDTHIS=$(echo "$TRAVIS_TAG"); fi
if [ -z "$TRAVIS_TAG" ]; then PKG_TAG=`date -u +%Y%m%d`; fi

export PKG_FILE="ghdl-$PKG_VER-$DBLD-$PKG_TAG-$PKG_DTAG-$PKG_SHORTCOMMIT.tgz"