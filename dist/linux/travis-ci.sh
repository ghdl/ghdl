#! /bin/bash
# This script is executed in the travis-ci environment.

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

# List of docker images

images=("ghdl/ghdl-tools:ubuntu-mcode"
	"ghdl/ghdl-tools:ubuntu1404-llvm"
	"ghdl/ghdl-tools:ubuntu1204-llvm"
	"ghdl/ghdl-tools:fedora-llvm-mcode")

# docker image index + identifier + compiler

regular="0+ubuntu+mcode 3+fedora+llvm"
nightly="1+ubuntu1404+llvm-3.5 2+ubuntu1204+llvm-3.8 3+fedora+mcode"
#release=()

# Selected build matrix
thismatrix=regular

#---

cloned=$(pwd)

#### Per build function

task() {
  printf "$ANSI_YELLOW[$1| BUILD] $2 $ANSI_NOCOLOR\n"

  IFS='+' read -ra REFS <<< "$2"
  DBLD=${REFS[2]}
  DDIST=${REFS[1]}
  DIMG=${images[${REFS[0]}]}
  thisworkdir="../wrk-$1"
  cp -r ./ "$thisworkdir" && cd "$thisworkdir"
  ./dist/linux/docker-buildtest.sh -i "$DIMG" $ENABLECOLOR -t "$1" -b "$DBLD" -f "ghdl-$PKG_TAG-$DBLD-$DDIST.tgz"
  cd "$cloned"
}

#### Start builds

printf "$ANSI_YELLOW[TRAVIS] Running matrix $thismatrix $ANSI_NOCOLOR\n"
eval blds='${'$thismatrix'}'
t=0; for thisbuild in $blds; do
  task "$t" "$thisbuild" &
  t=$(($t+1));
done

#### Wait end of builds

printf "$ANSI_YELLOW[TRAVIS] Waiting... $ANSI_NOCOLOR\n"
wait

#### Check results, disp logs

EXITCODE=0;
t=0; for b in $blds; do
  workdir="../wrk-$t"
  # Display log (with travis log folding commands)
  echo "travis_fold:start:log.$t"
  printf "$ANSI_YELLOW[TRAVIS] Print BUILD $t log $ANSI_NOCOLOR\n"
  cat $workdir/log.log
  echo "travis_fold:end:log.$t"

  # Read the last line of the log
  RESULT="$(tail -1 $workdir/log.log)"
  # If it did not end with [$t|SUCCESSFUL], break the build
  if [ "$RESULT" != "[$t|SUCCESSFUL]" ]; then
      printf "$ANSI_RED[TRAVIS] BUILD $t failed $ANSI_NOCOLOR\n"
      EXITCODE=1;
  else
      cp $workdir/ghdl-*.tgz .
  fi
  t=$(($t+1));
done

exit $EXITCODE
