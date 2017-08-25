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

PKG_SHORTCOMMIT="$(printf $TRAVIS_COMMIT | cut -c1-10)"
PKG_VER=`grep Ghdl_Ver src/version.in | sed -e 's/.*"\(.*\)";/\1/'`
PKG_TAG="$TRAVIS_TAG"
if [ -z "$TRAVIS_TAG" ]; then
    PKG_TAG="$(date -u +%Y%m%d)-$PKG_SHORTCOMMIT";
fi

# OS-X

if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    ./dist/macosx/install-ada.sh || exit 1
    PATH=$PWD/gnat/bin:$PATH
    DBLD=mcode
    DDIST=macosx
    ./dist/linux/buildtest.sh $ENABLECOLOR -t 0 -b "$DBLD" -f "ghdl-$PKG_TAG-$DBLD-$DDIST.tgz"
    exit
fi

if [ "$DOCKER_IMAGE" = "" ]; then
    echo "DOCKER_IMAGE not defined"
    exit 1
fi

IFS='+' read -ra REFS <<< "$DOCKER_IMAGE"
DBLD=${REFS[1]}
DDIST=${REFS[0]}

. ./dist/linux/travis-utils.sh

# Execute build and test in docker container
echo "travis_fold:start:create"
travis_time_start
printf "$ANSI_YELLOW[DOCKER build] Docker build $ANSI_NOCOLOR\n"

DOCKERFILE="dist/linux/docker/build-$DOCKER_IMAGE"
DOCKERCMD="./dist/linux/buildtest.sh $ENABLECOLOR -t 0 -b $DBLD -f ghdl-${PKG_TAG}-${DBLD}-${DDIST}.tgz"

echo "dockerfile: $DOCKERFILE"
echo "docker cmd: $DOCKERCMD"
DOCKER_NAME=`echo $DOCKER_IMAGE | sed -e 's/+/-/g'`

docker build -t $DOCKER_NAME - < $DOCKERFILE
travis_time_finish
echo "travis_fold:end:create"

docker run --rm --tty --volume $(pwd):/work -w "/work" $DOCKER_NAME bash -c "$DOCKERCMD"

ls -l ghdl-*

if [ ! -f build_ok ]; then
    printf "$ANSI_RED[TRAVIS] BUILD failed $ANSI_NOCOLOR\n"
    exit 1
fi
