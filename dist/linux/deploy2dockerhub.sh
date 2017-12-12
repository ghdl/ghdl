#! /bin/sh

set -e

. dist/linux/travis-utils.sh
. dist/ansi_color.sh
#disable_color

# Skip deploy if we are in stage 0 (i.e. IMAGE="") and there is no '[ci images]' in the commit message
if [ "$1" = "skip" ] || [ "${1}$(echo $2 | grep -o '\[ci images\]')" = "" ]; then
    printf "${ANSI_GREEN}SKIP DEPLOY2DOCKERHUB$ANSI_NOCOLOR\n";
    exit 0;
fi

case $1 in
  "")    FILTER="/";;
  *)     FILTER="/ghdl /pkg";;
esac

docker login -u="$DOCKER_USER" -p="$DOCKER_PASS"
for key in $FILTER; do
  for tag in `echo $(docker images ghdl$key* | awk -F ' ' '{print $1 ":" $2}') | cut -d ' ' -f2-`; do
      if [ "$tag" = "REPOSITORY:TAG" ]; then break; fi
      echo "travis_fold:start:`echo $tag | grep -oP 'ghdl/\K.*'`"
      travis_time_start
      printf "$ANSI_YELLOW[DOCKER push] ${tag}$ANSI_NOCOLOR\n"
      docker push $tag
      travis_time_finish
      echo "travis_fold:end:`echo $tag | grep -oP 'ghdl/\K.*'`"
  done
done
docker logout
