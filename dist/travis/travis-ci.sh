#! /bin/bash
# This script is executed in the travis-ci environment.

build_img_ghdl() {
    travis_start "build_run" "$ANSI_BLUE[DOCKER build] ghdl/ghdl:${IMAGE_TAG}$ANSI_NOCOLOR"
    docker build -t ghdl/ghdl:$IMAGE_TAG . -f-<<EOF
FROM ghdl/run:$IMAGE_TAG
ADD `ls | grep -oP 'ghdl-.*tgz'` /usr/local
EOF
    travis_finish "build_run"
}

#---

set -e

scriptdir=$(dirname $0)

. "$scriptdir/utils.sh"


# Display env (to debug)

travis_start "travis_env" "$ANSI_YELLOW[TRAVIS] Travis environment $ANSI_NOCOLOR"
env | grep TRAVIS
travis_finish "travis_env"


if [ "$IMAGE" = "" ]; then
    echo "IMAGE not defined"
    exit 1
fi


travis_start "fetch" "$ANSI_YELLOW[GHDL - build] git fetch --unshallow $ANSI_NOCOLOR"
# The command 'git describe' (used for version) needs the history. Get it.
# But the following command fails if the repository is complete.
git fetch --unshallow || true
travis_finish "fetch"


if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    travis_start "ada" "$ANSI_YELLOW[GHDL - build] Install gnat compiler (use cache) $ANSI_NOCOLOR"
    ./dist/macosx/install-ada.sh || exit 1
    PATH=$PWD/gnat/bin:$PATH
    travis_finish "ada"
fi


# Get build command options
travis_start "opts" "$ANSI_YELLOW[GHDL - build] Get build command options $ANSI_NOCOLOR"
buildCmdOpts
echo "build cmd: $BUILD_CMD_OPTS"
travis_finish "opts"

# Build

RUN="docker run --rm -t -e TRAVIS=$TRAVIS -v $(pwd):/work -w /work"

if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    bash -c "${scriptdir}/build.sh $BUILD_CMD_OPTS"
else
    # Assume linux

    travis_start "version" "$ANSI_YELLOW[GHDL - build] Build version.tmp and replace version.in with it (so that the version is correctly set) $ANSI_NOCOLOR"
    # This is a little bit hack-ish, as it assumes that 'git' is not
    # available in docker (otherwise it will describe as -dirty
    # because this modifies the source file version.in).
    ghdl_version_line=`grep -e '^ghdl_version' configure`
    make -f Makefile.in srcdir=. $ghdl_version_line version.tmp
    cp version.tmp src/version.in
    travis_finish "version"

    IMAGE_TAG=`echo $IMAGE | sed -e 's/+/-/g'`

    travis_start "pull" "$ANSI_YELLOW[GHDL - build] Docker pull ghdl/build:$IMAGE $ANSI_NOCOLOR"
    docker pull ghdl/build:$IMAGE_TAG
    travis_finish "pull"

    # Run build in docker
    $RUN "ghdl/build:$IMAGE_TAG" bash -c "${scriptdir}/build.sh $BUILD_CMD_OPTS"
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
    $RUN "ghdl/ghdl:$IMAGE_TAG" bash -c "GHDL=ghdl ${scriptdir}/test.sh $BUILD_CMD_OPTS"
fi

if [ ! -f test_ok ]; then
    printf "$ANSI_RED[TRAVIS] TEST failed $ANSI_NOCOLOR\n"
    exit 1
fi
