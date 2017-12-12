#! /bin/bash
# This script is executed in the travis-ci environment.

build_img_ghdl() {
    # Build ghdl/ghdl from ghdl/run
    echo "travis_fold:start:build_run"
    travis_time_start
    printf "$ANSI_BLUE[DOCKER build] ghdl : ${IMAGE_TAG}$ANSI_NOCOLOR\n"

    PKG=`ls | grep -oP 'ghdl-.*tgz'`
    mkdir tmp-img && cd tmp-img
    cp ../$PKG ./
    cp ../BUILD_TOOLS ./
    echo "FROM ghdl/run:$IMAGE_TAG" > Dockerfile
    echo "ADD $PKG /usr/local" >> Dockerfile
    docker build -t ghdl/ghdl:$IMAGE_TAG .
    cd ..
    travis_time_finish
    echo "travis_fold:end:build_run"
}

#---

set -e

. dist/linux/travis-utils.sh
. dist/ansi_color.sh
#disable_color

scriptdir=$(dirname $0)


# Display env (to debug)

echo -en "travis_fold:start:travis_env\r"
printf "$ANSI_YELLOW[TRAVIS] Travis environment $ANSI_NOCOLOR\n"
env | grep TRAVIS
echo -en "travis_fold:end:travis_env\r"


if [ "$IMAGE" = "" ]; then
    echo "IMAGE not defined"
    exit 1
fi


echo "travis_fold:start:patch_version"
# The command 'git describe' (used for version) needs the history. Get it.
# But the following command fails if the repository is complete.
git fetch --unshallow || true

# Build version.tmp and replace version.in with it (so that the version is
# correctly set).
make -f Makefile.in srcdir=. version.tmp
cp version.tmp src/version.in
echo "travis_fold:end:patch_version"


# Compute package name

PKG_SHORTCOMMIT="$(printf $TRAVIS_COMMIT | cut -c1-10)"
PKG_VER=`grep Ghdl_Ver src/version.in | sed -e 's/.*"\(.*\)";/\1/'`
if [ -z "$TRAVIS_TAG" ]; then
    # No tag: use date + commit id
    PKG_TAG="$(date -u +%Y%m%d)-$PKG_SHORTCOMMIT";
elif expr "$TRAVIS_TAG" : 'v[0-9].*' > /dev/null; then
    # Remove leading 'v' in tags in the filenames.
    PKG_TAG="$(echo $TRAVIS_TAG | cut -c2-)"
else
    PKG_TAG="$TRAVIS_TAG"
fi

IFS='+' read -ra REFS <<< "$IMAGE"
DDIST=${REFS[0]}
DBLD=${REFS[1]}
DGPL=${REFS[2]}

PKG_NAME="ghdl-${PKG_TAG}-${DBLD}-${DDIST}"
BUILD_CMD_OPTS="$ENABLECOLOR -b $DBLD"
if [ "$DGPL" = "gpl" ]; then
    BUILD_CMD_OPTS="$BUILD_CMD_OPTS --gpl"
    PKG_NAME="ghdl-gpl-${PKG_TAG}"
fi
BUILD_CMD_OPTS="${BUILD_CMD_OPTS} -p $PKG_NAME"

echo "build cmd: $BUILD_CMD_OPTS"


# Build

if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    # Install gnat compiler (use cache)
    ./dist/macosx/install-ada.sh || exit 1
    PATH=$PWD/gnat/bin:$PATH

    bash -c "${scriptdir}/build.sh $BUILD_CMD_OPTS"
else
    # Assume linux

    # Run build in docker
    IMAGE_TAG=`echo $IMAGE | sed -e 's/+/-/g'`
    docker run --rm -t -v $(pwd):/work -w "/work" ghdl/build:$IMAGE_TAG bash -c "${scriptdir}/build.sh $BUILD_CMD_OPTS"
fi

if [ ! -f build_ok ]; then
    printf "$ANSI_RED[TRAVIS] BUILD failed $ANSI_NOCOLOR\n"
    exit 1
fi


# Test

if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    bash -c "prefix=$(realpath ./install-mcode) ${scriptdir}/test.sh $BUILD_CMD_OPTS"
else
    # Build ghdl/ghdl:$IMAGE_TAG image
    build_img_ghdl
    # Run test in docker container
    docker run --rm -t -v $(pwd):/work -w "/work" ghdl/ghdl:$IMAGE_TAG bash -c "GHDL=ghdl ${scriptdir}/test.sh $BUILD_CMD_OPTS"
fi

if [ ! -f test_ok ]; then
    printf "$ANSI_RED[TRAVIS] TEST failed $ANSI_NOCOLOR\n"
    exit 1
fi
