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
    "--grab"|"-grab")     set -- "$@" "-g";;
	"--image"|"-image")   set -- "$@" "-i";;
	"--build"|"-build")   set -- "$@" "-b";;
	"--file"|"-file")     set -- "$@" "-f";;
	"--taskid"|"-taskid") set -- "$@" "-t";;
    *) set -- "$@" "$arg"
  esac
done
# Parse args
while getopts ":i:b:f:t:cg" opt; do
  case $opt in
    c) enable_color;;
    g) GRAB_SRCS=1;;
    i) DOCKER_IMG=$OPTARG;;
    b) BLD=$OPTARG ;;
    f) PKG_FILE=$OPTARG;;
    t) TASK=$OPTARG;;
    \?) printf "$ANSI_RED[BUILD] Invalid option: -$OPTARG $ANSI_NOCOLOR\n" >&2; exit 1 ;;
    :)  printf "$ANSI_RED[BUILD] Option -$OPTARG requires an argument $ANSI_NOCOLOR\n" >&2; exit 1 ;;
  esac
done

#---

printf "$ANSI_YELLOW[$TASK| BUILD] Docker pull $DOCKER_IMG $ANSI_NOCOLOR\n"
docker pull "$DOCKER_IMG" > /dev/null 2>&1

#---

printf "$ANSI_YELLOW[$TASK| BUILD] Docker run $DOCKER_IMG $BLD $PKG_FILE $ANSI_NOCOLOR\n"

if [ -n "$GRAB_SRCS" ]; then

  printf "$ANSI_YELLOW[$TASK| BUILD] Grab sources$ANSI_NOCOLOR\n"
  
  p="mkdir /work && cd /work"
  p="$p && curl -L https://github.com/tgingold/ghdl/archive/master.tar.gz | tar xz"
  p="$p && mv ghdl-master/* ./ && rm -rf ghdl-master"

  set +e
  docker run --name ghdl_cmp -t "$DOCKER_IMG" sh -c "$p &&./dist/linux/buildtest.sh $ENABLECOLOR-t $TASK -b $BLD -f $PKG_FILE"
  docker cp "ghdl_cmp:/work/log.log" "./log.log"
  set -e
  docker cp "ghdl_cmp:/work/$PKG_FILE" ./
  docker rm ghdl_cmp
  
else

  docker run --rm -tv $(pwd):/work:Z -w="/work" "$DOCKER_IMG" sh -c "./dist/linux/buildtest.sh $ENABLECOLOR-t $TASK -b $BLD -f $PKG_FILE"

fi