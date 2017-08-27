#! /bin/bash
# This script is executed in the travis-ci environment.

set -e

. dist/ansi_color.sh
#disable_color


# Display env (to debug)

echo -en "travis_fold:start:travis_env\r"
printf "$ANSI_YELLOW[TRAVIS] Travis environment $ANSI_NOCOLOR\n"
env | grep TRAVIS
echo -en "travis_fold:end:travis_env\r"


# Compute package name

PKG_SHORTCOMMIT="$(printf $TRAVIS_COMMIT | cut -c1-10)"
PKG_VER=`grep Ghdl_Ver src/version.in | sed -e 's/.*"\(.*\)";/\1/'`
PKG_TAG="$TRAVIS_TAG"
if [ -z "$TRAVIS_TAG" ]; then
    PKG_TAG="$(date -u +%Y%m%d)-$PKG_SHORTCOMMIT";
fi

if [ "$IMAGE" = "" ]; then
    echo "IMAGE not defined"
    exit 1
fi

IFS='+' read -ra REFS <<< "$IMAGE"
DDIST=${REFS[0]}
DBLD=${REFS[1]}
DGPL=${REFS[2]}

PKG_NAME="ghdl-${PKG_TAG}-${DBLD}-${DDIST}"
BUILD_CMD="./dist/linux/buildtest.sh $ENABLECOLOR -b $DBLD"
if [ "$DGPL" = "gpl" ]; then
    BUILD_CMD="$BUILD_CMD --gpl"
    PKG_NAME="ghdl-gpl-${PKG_TAG}"
fi
BUILD_CMD="${BUILD_CMD} -p $PKG_NAME"

echo "build cmd: $BUILD_CMD"

# Build

if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    # Install gnat compiler (use cache)
    ./dist/macosx/install-ada.sh || exit 1
    PATH=$PWD/gnat/bin:$PATH

    bash -c "$BUILD_CMD"
else
    # Assume linux

    # Create docker image

    . ./dist/linux/travis-utils.sh

    echo "travis_fold:start:create"
    travis_time_start
    printf "$ANSI_YELLOW[DOCKER build] Docker build $ANSI_NOCOLOR\n"

    DOCKERFILE="dist/linux/docker/build-$IMAGE"

    echo "dockerfile: $DOCKERFILE"
    DOCKER_NAME=`echo $IMAGE | sed -e 's/+/-/g'`

    docker build -t $DOCKER_NAME - < $DOCKERFILE
    travis_time_finish
    echo "travis_fold:end:create"


    # Run build+test in docker

    docker run --rm --tty --volume $(pwd):/work -w "/work" $DOCKER_NAME bash -c "$BUILD_CMD"
fi


ls -l ghdl-*

if [ ! -f build_ok ]; then
    printf "$ANSI_RED[TRAVIS] BUILD failed $ANSI_NOCOLOR\n"
    exit 1
fi
