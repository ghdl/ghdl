#! /bin/bash

scriptdir=`dirname $0`

if [ -n "$GITHUB_EVENT_PATH" ]; then
  export CI=true
fi

. "$scriptdir/ansi_color.sh"
disable_color

print_start() {
  COL="$ANSI_YELLOW"
  if [ "x$2" != "x" ]; then
    COL="$2"
  fi
  printf "${COL}${1}$ANSI_NOCOLOR\n"
}

gstart () {
  print_start "$@"
}
gend () {
  :
}

if [ -n "$TRAVIS" ]; then
  echo "INFO: set 'gstart' and 'gend' for TRAVIS"
  # This is a trimmed down copy of https://github.com/travis-ci/travis-build/blob/master/lib/travis/build/bash/*
  travis_time_start() {
    # `date +%N` returns the date in nanoseconds. It is used as a replacement for $RANDOM, which is only available in bash.
    travis_timer_id=`date +%N`
    travis_start_time=`travis_nanoseconds`
    echo "travis_time:start:$travis_timer_id"
  }
  travis_time_finish() {
    travis_end_time=`travis_nanoseconds`
    local duration=$(($travis_end_time-$travis_start_time))
    echo "travis_time:end:$travis_timer_id:start=$travis_start_time,finish=$travis_end_time,duration=$duration"
  }

  if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    travis_nanoseconds() {
      date -u '+%s000000000'
    }
  else
    travis_nanoseconds() {
      date -u '+%s%N'
    }
  fi

  gstart () {
    echo "travis_fold:start:group"
    travis_time_start
    print_start "$@"
  }

  gend () {
    travis_time_finish
    echo "travis_fold:end:group"
  }
else
  if [ -n "$CI" ]; then
    echo "INFO: set 'gstart' and 'gend' for CI"
    gstart () {
      printf '::group::'
      print_start "$@"
      SECONDS=0
    }

    gend () {
      duration=$SECONDS
      echo '::endgroup::'
      printf "${ANSI_GRAY}took $(($duration / 60)) min $(($duration % 60)) sec.${ANSI_NOCOLOR}\n"
    }
  fi
fi

echo "cliargs: $0 $@"

# Stop in case of error
set -e

ISGPL=false
ISSYNTH=false

# Transform long options to short ones
for arg in "$@"; do
  shift
  case "$arg" in
      "--color"|"-color")     set -- "$@" "-c";;
      "--backend"|"-backend") set -- "$@" "-b";;
      "--pkg"|"-pkg")         set -- "$@" "-p";;
      "--gpl"|"-gpl")         set -- "$@" "-g";;
      "--synth"|"-synth")     set -- "$@" "-s";;
    *) set -- "$@" "$arg"
  esac
done
# Parse args
while getopts ":b:p:cgs" opt; do
  case $opt in
    c) enable_color;;
    b) BACK=$OPTARG ;;
    p) PKG_NAME=$OPTARG;;
    g) ISGPL=true;;
    s) ISSYNTH=true;;
    \?) printf "$ANSI_RED[CI - args] Invalid option: -$OPTARG $ANSI_NOCOLOR\n" >&2
        exit 1 ;;
    :)  printf "$ANSI_RED[CI - args] Option -$OPTARG requires an argument. $ANSI_NOCOLOR\n" >&2
        exit 1 ;;
  esac
done
shift $((OPTIND -1))

#---

#
# Build command options
#

notag() {
  # No tag: use date + commit id
  echo "`git log -1 --date=short --pretty=format:%cd | sed 's/-//g'`-$PKG_SHA"
}

vertag() {
  if expr "$1" : 'v[0-9].*' > /dev/null; then
    # Remove leading 'v' in tags in the filenames.
    echo $1 | cut -c2-
    # Check version defined in configure.
    if [ "x$1" != "x`grep "^ghdl_version=" configure | sed -e 's/.*"\(.*\)";/\1/'`" ]; then
      printf "${ANSI_RED}Tag '$1' does not match 'ghdl_version'!${ANSI_NOCOLOR}\n" 1>&2;
      exit 1
    fi
  else
    # Regular tag (like snapshots), nothing to change.
    echo "$2"
  fi
}

buildCmdOpts () {
  BUILD_ARG="$1"

  # Get short commit SHA
  if [ -n "$TRAVIS_COMMIT" ]; then
    GIT_SHA="$TRAVIS_COMMIT"
  fi
  if [ -n "$GITHUB_SHA" ]; then
    GIT_SHA="$GITHUB_SHA"
  fi
  if [ -z "$GIT_SHA" ]; then
    GIT_SHA="`git rev-parse --verify HEAD`"
  fi
  PKG_SHA="`printf $GIT_SHA | cut -c1-10`"

  echo "TRAVIS_COMMIT: $TRAVIS_COMMIT"
  echo "TRAVIS_TAG: $TRAVIS_TAG"
  echo "GITHUB_SHA: $GITHUB_SHA"
  echo "GITHUB_REF: $GITHUB_REF"
  echo "GIT_SHA: $GIT_SHA"

  # Compute package name
  case "$GITHUB_REF" in
    *tags*)
      PKG_TAG="$(vertag "`echo "$GITHUB_REF" | sed 's#^refs/tags/\(.*\)#\1#g'`" "$GITHUB_REF")"
    ;;
    *heads*|*pull*)
      PKG_TAG="`notag`"
    ;;
    "")
      if [ -z "$TRAVIS_TAG" ]; then
        PKG_TAG="`notag`"
      else
        PKG_TAG="`vertag "$TRAVIS_TAG" "$TRAVIS_TAG"`"
      fi
    ;;
    *)
      PKG_TAG="$GITHUB_REF"
    ;;
  esac

  echo "PKG_SHA: $PKG_SHA"
  echo "PKG_TAG: $PKG_TAG"

  # Extract from BUILD_ARG
  IFS='+' read -ra REFS <<< "$BUILD_ARG"
  DDIST=${REFS[0]}  # Linux distro (eg: ubuntuXX, fedoraXX)
  DBACK=${REFS[1]}  # Build/backend (eg: mcode, llvm)

  PKG_NAME="ghdl-${PKG_TAG}-${DDIST}-${DBACK}"
  BUILD_CMD_OPTS="$ENABLECOLOR -b $DBACK"

  if [ "x$ISGPL" = "xtrue" ]; then
      BUILD_CMD_OPTS="$BUILD_CMD_OPTS --gpl"
      PKG_NAME="${PKG_NAME}-gpl"
      DEXT="-gpl"
  fi
  if [ "x$ISSYNTH" = "xtrue" ]; then
      BUILD_CMD_OPTS="$BUILD_CMD_OPTS --synth"
      PKG_NAME="${PKG_NAME}-synth"
      DEXT="-synth"
  fi
  export BUILD_CMD_OPTS="${BUILD_CMD_OPTS} -p $PKG_NAME"

  GHDL_IMAGE_TAG="`echo $BUILD_ARG | sed -e 's/+/-/g'`"
  BUILD_IMAGE_TAG="$GHDL_IMAGE_TAG"

  case $BUILD_ARG in
    *gcc*)
      BUILD_IMAGE_TAG="`echo $GHDL_IMAGE_TAG | sed 's#\(.*\)-gcc.*#\1-gcc#g'`"
    ;;
  esac

  GHDL_IMAGE_TAG="${GHDL_IMAGE_TAG}$DEXT"
}

run_cmd() {
  echo "$@"
  "$@"
}

#
# Build ghdl
#

build () {
  rm -f build_ok

  #--- Env

  gstart "[GHDL - build] Environment"
  env
  gend

  #--- GPL: gpl-ize sources

  if [ "$ISGPL" = "true" ]; then
      GPLDIR="${PKG_NAME}.src"
      gstart "[GHDL - build] create GPL source package (${ANSI_CYAN}${GPLDIR}.tgz${ANSI_NOCOLOR})"
      files=`echo *`
      make -f Makefile.in srcdir=. clean-pure-gpl
      mkdir "$GPLDIR"
      cp -pdrl $files "$GPLDIR"
      tar -zcf "${GPLDIR}.tgz" "$GPLDIR"
      gend
  fi

  #--- Configure

  CDIR=`pwd`
  export prefix="$CDIR/install-$BACK"
  mkdir "$prefix"
  mkdir "build-$BACK"
  cd "build-$BACK"

  if [ "x$ISSYNTH" = "xtrue" ]; then
    CONFIG_OPTS+=" --enable-synth"
  fi

  case "$BACK" in
      gcc*)
          gstart "[GHDL - build] Get gcc sources"
          echo "https://github.com/gcc-mirror/gcc/archive/`echo ${BACK} | sed -e 's/\./_/g'`-release.tar.gz"
          mkdir gcc-srcs
          curl -L "https://github.com/gcc-mirror/gcc/archive/`echo ${BACK} | sed -e 's/\./_/g'`-release.tar.gz" | tar -xz -C gcc-srcs --strip-components=1
          cd gcc-srcs
          sed -i.bak s/ftp:/http:/g ./contrib/download_prerequisites
          ./contrib/download_prerequisites
          cd ..
          gend

          gstart "[GHDL - build] Configure ghdl"
          run_cmd ../configure --with-gcc=gcc-srcs --prefix="$prefix" $CONFIG_OPTS
          gend
          gstart "[GHDL - build] Copy sources"
          make copy-sources
          mkdir gcc-objs; cd gcc-objs
          gend
          gstart "[GHDL - build] Configure gcc"
          run_cmd ../gcc-srcs/configure --prefix="$prefix" --enable-languages=c,vhdl --disable-bootstrap --disable-lto --disable-multilib --disable-libssp --disable-libgomp --disable-libquadmath "`gcc -v 2>&1 | grep -o -- --enable-default-pie`"
          gend
      ;;
      mcode)
          CXX=""
      ;;
      llvm)
          CXX="clang"
          CONFIG_OPTS+=" --with-llvm-config CXX=$CXX"
      ;;
      llvm-3.5)
          CXX="clang++"
          CONFIG_OPTS+=" --with-llvm-config=llvm-config-3.5 CXX=$CXX"
      ;;
      llvm-*)
          llvmver=`echo $BACK | sed -e "s/llvm-//"`
          CXX="clang++-$llvmver"
          CONFIG_OPTS+=" --with-llvm-config=llvm-config-$llvmver CXX=$CXX"
      ;;
      *)
          printf "$ANSI_RED[GHDL - build] Unknown build $BACK $ANSI_NOCOLOR\n"
          exit 1;;
  esac

  if [ ! "`echo $BACK | grep gcc`" ]; then
      gstart "[GHDL - build] Configure"
      run_cmd ../configure "--prefix=$prefix" $CONFIG_OPTS
      gend
  fi

  #--- make

  gstart "[GHDL - build] Make"
  set +e
  make LIB_CFLAGS="$LIB_CFLAGS" OPT_FLAGS="$OPT_FLAGS" -j`nproc` 2>make_err.log
  tail -1000 make_err.log
  set -e
  gend

  gstart "[GHDL - build] Install"
  make install
  cd ..
  gend

  if [ "`echo $BACK | grep gcc`" ]; then
      gstart "[GHDL - build] Make ghdllib"
      make ghdllib
      gend

      gstart "[GHDL - build] Install ghdllib"
      make install
      cd ..
      gend
  fi

  #--- package

  gstart "[GHDL - build] Create package ${ANSI_DARKCYAN}${PKG_NAME}.tgz"
  tar -zcvf "${PKG_NAME}.tgz" -C "$prefix" .
  gend

  #--- build tools versions

  {
      make --version | grep 'Make'
      gnatls --version | grep 'GNATLS'
      gcc --version | grep 'gcc'
      if [ "$CXX" != "" ]; then
          $CXX --version | grep 'clang'
      fi
  } > BUILD_TOOLS

  #---

  printf "$ANSI_GREEN[GHDL - build] SUCCESSFUL${ANSI_NOCOLOR}\n"
  touch build_ok
}

#
# Build ghdl/ghdl image
#

build_img_ghdl() {
    gstart "[DOCKER - build] ghdl/ghdl:${GHDL_IMAGE_TAG}" "$ANSI_BLUE"
    docker build -t ghdl/ghdl:$GHDL_IMAGE_TAG . -f-<<EOF
FROM ghdl/run:$BUILD_IMAGE_TAG
ADD `ls | grep -v '\.src\.' | grep '^ghdl.*\.tgz'` /usr/local
EOF
    gend
}

#
# Full CI run
#

ci_run () {
  if [ "x$TASK" = "x" ]; then
    if [ "x$1" = "x" ]; then
      printf "${ANSI_RED}TASK not defined${ANSI_NOCOLOR}\n"
      exit 1
    else
      TASK="$1"
    fi
  fi

  gstart "[CI] git fetch --unshallow" "$ANSI_BLUE"
  # The command 'git describe' (used for version) needs the history. Get it.
  # But the following command fails if the repository is complete.
  git fetch --unshallow || true
  gend

  # Get build command options
  gstart "[CI] Get build command options" "$ANSI_BLUE"
  buildCmdOpts "$TASK"
  echo "build cmd: $BUILD_CMD_OPTS"
  gend

  # Build

  RUN="docker run --rm -t -e CI -e TRAVIS -v `pwd`:/work -w /work"
  if [ "x$IS_MACOS" = "xtrue" ]; then
      export CPATH="$CPATH:`xcrun --show-sdk-path`/usr/include"
      CC=clang \
      CONFIG_OPTS="--disable-libghdl" \
      bash -c "${scriptdir}/ci-run.sh $BUILD_CMD_OPTS build"
  else
      # Assume linux

      gstart "[CI] Build version.tmp and replace version.in with it (so that the version is correctly set)" "$ANSI_BLUE"
      # This is a little bit hack-ish, as it assumes that 'git' is not
      # available in docker (otherwise it will describe as -dirty
      # because this modifies the source file version.in).
      ghdl_version_line=`grep -e '^ghdl_version' configure`
      make -f Makefile.in srcdir=. $ghdl_version_line version.tmp
      cp version.tmp src/version.in
      gend

      gstart "[CI] Docker pull ghdl/build:$BUILD_IMAGE_TAG" "$ANSI_BLUE"
      docker pull ghdl/build:$BUILD_IMAGE_TAG
      gend

      printf "$ANSI_BLUE[CI] Build ghdl in docker image ghdl/build:$BUILD_IMAGE_TAG\n"
      $RUN -e CONFIG_OPTS="$CONFIG_OPTS" "ghdl/build:$BUILD_IMAGE_TAG" bash -c "${scriptdir}/ci-run.sh $BUILD_CMD_OPTS build"
  fi

  if [ ! -f build_ok ]; then
      printf "${ANSI_RED}[GHDL - build] FAILED${ANSI_NOCOLOR}\n"
      exit 1
  fi

  # Test

  if [ "x$IS_MACOS" = "xtrue" ]; then
      CC=clang \
      prefix="`cd ./install-mcode; pwd`" \
      ./testsuite/testsuite.sh sanity gna vests
  else
      # Build ghdl/ghdl:$GHDL_IMAGE_TAG image
      build_img_ghdl
      # Run test in docker container
      tests="sanity"
      if [ "x$ISGPL" != "xtrue" ]; then
        tests="$tests gna"
      fi
      tests="$tests vests"
      if [ "x$ISSYNTH" = "xtrue" ]; then
        tests="$tests synth"
      fi
      $RUN "ghdl/ghdl:$GHDL_IMAGE_TAG" bash -c "GHDL=ghdl ./testsuite/testsuite.sh $tests"
  fi

  if [ ! -f testsuite/test_ok ]; then
      printf "${ANSI_RED}[GHDL - test] FAILED${ANSI_NOCOLOR}\n"
      exit 1
  fi
}

#---

echo "command: $0 $@"

unset IS_MACOS
if [ "$GITHUB_OS" = "macOS" ] || [ "$TRAVIS_OS_NAME" = "osx" ]; then
  IS_MACOS="true"
fi

case "$1" in
  build)
    build
  ;;
  *)
    ci_run
  ;;
esac
