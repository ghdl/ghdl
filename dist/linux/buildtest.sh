#! /bin/sh

. dist/ansi_color.sh
disable_color

# Stop in case of error
set -e

# Transform long options to short ones
for arg in "$@"; do
  shift
  case "$arg" in
    "--color"|"-color")   set -- "$@" "-c";;
	"--build"|"-build")   set -- "$@" "-b";;
	"--file"|"-file")     set -- "$@" "-f";;
	"--taskid"|"-taskid") set -- "$@" "-t";;
    *) set -- "$@" "$arg"
  esac
done
# Parse args
while getopts ":b:f:t:c" opt; do
  case $opt in
    c) enable_color;;
    b) BLD=$OPTARG ;;
	f) PKG_FILE=$OPTARG;;
    t) TASK=$OPTARG;;
    \?) printf "$ANSI_RED[GHDL] Invalid option: -$OPTARG $ANSI_NOCOLOR\n" >&2; exit 1 ;;
    :)  printf "$ANSI_RED[GHDL] Option -$OPTARG requires an argument. $ANSI_NOCOLOR\n" >&2; exit 1 ;;
  esac
done

#---

printf "$ANSI_BLUE[$TASK| GHDL] Prepare $(pwd) $ANSI_NOCOLOR\n"
CDIR=$(pwd)
mkdir logs
prefix="$CDIR/install-$BLD"
mkdir "$prefix"
mkdir "build-$BLD"
cd "build-$BLD"

#---

printf "$ANSI_BLUE[$TASK| GHDL] Environment $ANSI_NOCOLOR\n"
env 1>> ../log.log 2>&1

#---

printf "$ANSI_BLUE[$TASK| GHDL - build] Configure $ANSI_NOCOLOR\n"
case "$BLD" in
  mcode)
    ../configure "--prefix=$prefix" 1>> ../log.log 2>&1
  ;;
	  
  llvm)
    ../configure "--prefix=$prefix" "--with-llvm-config" 1>> ../log.log 2>&1
  ;;
	  
  llvm-3.5)
    ../configure "--prefix=$prefix" "--with-llvm-config=llvm-config-3.5" 1>> ../log.log 2>&1
    MAKEOPTS="CXX=clang++"
  ;;

  llvm-3.8)
    ../configure "--prefix=$prefix" "--with-llvm-config=llvm-config-3.8"  1>> ../log.log 2>&1
    MAKEOPTS="CXX=clang++-3.8"
  ;;

  docker) printf "$ANSI_MAGENTA[$TASK| GHDL - build] Check docker container! $ANSI_NOCOLOR\n"; exit 0;;

  *)      printf "$ANSI_RED[$TASK| GHDL - build] Unknown build $BLD $ANSI_NOCOLOR\n"
          exit 1;;
esac

#---

printf "$ANSI_BLUE[$TASK| GHDL - build] Make $ANSI_NOCOLOR\n"
make  $MAKEOPTS 1>> ../log.log 2>&1
printf "$ANSI_BLUE[$TASK| GHDL - build] Install $ANSI_NOCOLOR\n"
make install 1>> ../log.log 2>&1
cd ..

#---

printf "$ANSI_BLUE[$TASK| GHDL] Create package $ANSI_DARKCYAN$PKG_FILE $ANSI_NOCOLOR\n"
tar -zcvf "$PKG_FILE" -C "$prefix" . 1>> log.log 2>&1

#---

export ENABLECOLOR="$ENABLECOLOR"
export TASK="$TASK"
export GHDL="$CDIR/install-$BLD/bin/ghdl"
cd testsuite && ./testsuite.sh
cd ..

#---

# Do not remove this line, and don't write anything below, since it is used to identify successful builds
echo "[$TASK|SUCCESSFUL]" 1>> log.log 2>&1