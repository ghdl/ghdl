#! /bin/sh
# This script is executed in the travis-ci environment.

images=("ghdl/ghdl-tools:ubuntu-mcode" "ghdl/ghdl-tools:ubuntu1404-llvm" "ghdl/ghdl-tools:ubuntu1204-llvm" "ghdl/ghdl-tools:fedora-llvm-mcode")

#--- image + identifier + compiler
	   
regular="0+ubuntu+mcode 3+fedora+llvm"
nightly="1+ubuntu1404+llvm-3.5 2+ubuntu1204+llvm-3.8 3+fedora+mcode"
#release=()

#---

. dist/ansi_color.sh

#disable_color

thismatrix=regular

#---

task() {
  printf "$ANSI_YELLOW[$1| BUILD] $2 $ANSI_NOCOLOR\n"

  IFS='+' read -ra REFS <<< "$2"
  DBLD=${REFS[2]}
  thisworkdir="../wrk-$1"
  cp -r ./ "$thisworkdir" && cd "$thisworkdir"
  ./dist/linux/docker-buildtest.sh -i "${images[${REFS[0]}]}" $3-t "$1" -b "$DBLD" -f "ghdl-$PKG_VER-$DBLD-$PKG_TAG-${REFS[1]}-$PKG_SHORTCOMMIT.tgz"
  cd "$cloned"
  cp "$thisworkdir"/ghdl-*.tgz ./
  cp "$thisworkdir"/log.log "./log_$1.log"
}

#---

PKG_SHORTCOMMIT="$(printf $TRAVIS_COMMIT | cut -c1-10)"
PKG_VER=`grep Ghdl_Ver src/version.in | sed -e 's/.*"\(.*\)";/\1/'`
PKG_TAG="$TRAVIS_TAG"
if [ -z "$TRAVIS_TAG" ]; then PKG_TAG=`date -u +%Y%m%d`; fi

cloned=$(pwd)

printf "$ANSI_YELLOW[TRAVIS] Running matrix $thismatrix $ANSI_NOCOLOR\n"
eval blds='${'$thismatrix'}'
t=0; for thisbuild in $blds; do
  task "$t" "$thisbuild" "$ENABLECOLOR" &
  t=$(($t+1));
done

printf "$ANSI_YELLOW[TRAVIS] Waiting... $ANSI_NOCOLOR\n"
wait
printf "$ANSI_YELLOW[TRAVIS] Done waiting. Show work dir content: $ANSI_NOCOLOR\n"
ls -la
printf "$ANSI_YELLOW[TRAVIS] Check results $ANSI_NOCOLOR\n"
EXITCODE=0;
t=0; for b in $blds; do
  # Read the last line of the log
  RESULT[$t]=$(awk '/./{line=$0} END{print line}' "log_$t.log")
  # If it did not end with [$t|SUCCESSFUL], break the build
  if [ "${RESULT[$t]}" != "[$t|SUCCESSFUL]" ]; then EXITCODE=$(($t+1)); fi
  # Anyway, always print the full log
  printf "$ANSI_YELLOW[TRAVIS] Print BUILD $t log $ANSI_NOCOLOR\n"
  cat "log_$t.log"
  t=$(($t+1));
done

# The exit code indicates the last broken build (1:bnum)
exit $EXITCODE