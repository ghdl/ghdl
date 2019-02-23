travis_start () {
  :
}
travis_finish () {
  :
}

if [ -n "$TRAVIS" ]; then
  # This is a trimmed down copy of https://github.com/travis-ci/travis-build/blob/master/lib/travis/build/bash/*
  travis_time_start() {
    # `date +%N` returns the date in nanoseconds. It is used as a replacement for $RANDOM, which is only available in bash.
    travis_timer_id=`date +%N`
    travis_start_time=$(travis_nanoseconds)
    echo "travis_time:start:$travis_timer_id"
  }
  travis_time_finish() {
    travis_end_time=$(travis_nanoseconds)
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

  travis_start () {
    echo "travis_fold:start:$1"
    if [ -z "$3" ]; then
      travis_time_start
    fi
    printf "$2\n"
  }

  travis_finish () {
    if [ -z "$2" ]; then
      travis_time_finish
    fi
    echo "travis_fold:end:$1"
  }
fi

#--

buildCmdOpts () {
  # Compute package name
  PKG_SHORTCOMMIT="$(printf $TRAVIS_COMMIT | cut -c1-10)"
  PKG_VER=`grep "ghdl_version=" configure | sed -e 's/.*"\(.*\)";/\1/'`
  if [ -z "$TRAVIS_TAG" ]; then
      # No tag: use date + commit id
      PKG_TAG="$(date -u +%Y%m%d)-$PKG_SHORTCOMMIT";
  elif expr "$TRAVIS_TAG" : 'v[0-9].*' > /dev/null; then
      # Remove leading 'v' in tags in the filenames.
      PKG_TAG="$(echo $TRAVIS_TAG | cut -c2-)"
  else
      # Regular tag (like snapshots), nothing to change.
      PKG_TAG="$TRAVIS_TAG"
  fi

  # Extract from IMAGE (defined in .travis.yml)
  IFS='+' read -ra REFS <<< "$IMAGE"
  DDIST=${REFS[0]}  # Linux distro (eg: ubuntuXX, fedoraXX)
  DBLD=${REFS[1]}   # Build/backend (eg: mcode, llvm)
  DGPL=${REFS[2]}   # GPL or not

  PKG_NAME="ghdl-${PKG_TAG}-${DDIST}-${DBLD}"
  BUILD_CMD_OPTS="$ENABLECOLOR -b $DBLD"
  if [ "$DGPL" = "gpl" ]; then
      BUILD_CMD_OPTS="$BUILD_CMD_OPTS --gpl"
      PKG_NAME="ghdl-gpl-${PKG_TAG}"
  fi
  export BUILD_CMD_OPTS="${BUILD_CMD_OPTS} -p $PKG_NAME"
}

#--

. "$scriptdir/../ansi_color.sh"
#disable_color
