echo "KAIXO"
echo "$DOCKER_IMG"

PKG_VER=`grep Ghdl_Ver src/version.in | sed -e 's/.*"\(.*\)";/\1/'`

if [ "$TRAVIS_TAG" = "" ]; then
    PKG_TAG=`date -u +%Y%m%d`
else
    PKG_TAG="$TRAVIS_TAG"
fi

IFS='+' read -ra REFS <<< "$BLD"

DBLD=${REFS[1]}
PKG_DTAG=${REFS[0]}

PKG_SHORTCOMMIT="$(echo $TRAVIS_COMMIT | cut -c1-10)"

export PKG_FILE="ghdl-$PKG_VER-$DBLD-$PKG_TAG-$PKG_DTAG-$PKG_SHORTCOMMIT.tgz"
