set -ev

docker pull "$DOCKER_IMG"

if [ -z "$DONTGRAB_SRCS" ]; then

  p="mkdir /work && cd /work"
  p="$p && curl -L https://github.com/tgingold/ghdl/archive/master.tar.gz | tar xz"
  p="$p && mv ghdl-master/* ./ && rm -rf ghdl-master"
  
  docker run --name ghdl_cmp -it "$DOCKER_IMG" sh -c "$p && sh ./dist/linux/buildtest.sh -b $DBLD -f $PKG_FILE"
  docker cp "ghdl_cmp:/work/$PKG_FILE" ./
  
else

  docker run --name ghdl_cmp -itv $(pwd):/work:Z "$DOCKER_IMG" sh -c "cd /work && sh ./dist/linux/buildtest.sh -b $DBLD -f $PKG_FILE"

fi

docker rm ghdl_cmp


