#! /bin/bash

scriptdir=$(dirname $0)

. "$scriptdir/utils.sh"
disable_color

echo "$0" "$@"

# Stop in case of error
set -e

# Transform long options to short ones
for arg in "$@"; do
  shift
  case "$arg" in
      "--color"|"-color")   set -- "$@" "-c";;
      "--build"|"-build")   set -- "$@" "-b";;
      "--pkg"|"-pkg")       set -- "$@" "-p";;
      "--gpl"|"-gpl")       set -- "$@" "-g";;
    *) set -- "$@" "$arg"
  esac
done
# Parse args
while getopts ":b:p:cg" opt; do
  case $opt in
    c) enable_color;;
    b) BLD=$OPTARG ;;
    p) PKG_NAME=$OPTARG;;
    g) ISGPL=true;;
    \?) printf "$ANSI_RED[GHDL - build] Invalid option: -$OPTARG $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
    :)  printf "$ANSI_RED[GHDL - build] Option -$OPTARG requires an argument. $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
  esac
done

rm -f build_ok

#--- Env

travis_start "env.docker" "$ANSI_YELLOW[Info] Environment $ANSI_NOCOLOR"
env
travis_finish "env.docker"

#--- GPL: gpl-ize sources

if [ "$ISGPL" = "true" ]; then
    travis_start "gpl.src" "$ANSI_YELLOW[Source] create GPL sources $ANSI_NOCOLOR"
    files=`echo *`
    make -f Makefile.in srcdir=. clean-pure-gpl
    mkdir ${PKG_NAME}
    cp -pdrl $files ${PKG_NAME}
    tar -zcf "${PKG_NAME}.tar.gz" ${PKG_NAME}
    PKG_NAME="${PKG_NAME}-${BLD}"
    travis_finish "gpl.src"
fi

#--- Configure

travis_start "configure" "$ANSI_YELLOW[GHDL - build] Configure $ANSI_NOCOLOR" -notime

CDIR=$(pwd)
export prefix="$CDIR/install-$BLD"
mkdir "$prefix"
mkdir "build-$BLD"
cd "build-$BLD"

case "$BLD" in
    gcc*)
        travis_start "get_gcc" "$ANSI_YELLOW[GHDL] Get gcc sources $ANSI_NOCOLOR"
        echo "https://github.com/gcc-mirror/gcc/archive/$(echo ${BLD} | sed -e 's/\./_/g')-release.tar.gz"
        mkdir gcc-srcs
        curl -L "https://github.com/gcc-mirror/gcc/archive/$(echo ${BLD} | sed -e 's/\./_/g')-release.tar.gz" | tar -xz -C gcc-srcs --strip-components=1
        cd gcc-srcs
        sed -i.bak s/ftp:/http:/g ./contrib/download_prerequisites
        ./contrib/download_prerequisites
        cd ..
        travis_finish "get_gcc"

        travis_start "configure_gcc" "$ANSI_YELLOW[GHDL] Configure gcc $ANSI_NOCOLOR"
        ../configure --with-gcc=gcc-srcs --prefix="$prefix"
        make copy-sources
        mkdir gcc-objs; cd gcc-objs
        ../gcc-srcs/configure --prefix="$prefix" --enable-languages=c,vhdl --disable-bootstrap --disable-lto --disable-multilib --disable-libssp --disable-libgomp --disable-libquadmath "`gcc -v 2>&1 | grep -o -- --enable-default-pie`"
        travis_finish "configure_gcc"
    ;;
    mcode)
	      config_opts=""
        CXX=""
    ;;
    llvm)
	      CXX="clang"
	      config_opts="--with-llvm-config CXX=$CXX"
    ;;
    llvm-3.5)
	      CXX="clang++"
	      config_opts="--with-llvm-config=llvm-config-3.5 CXX=$CXX"
    ;;
    llvm-3.8)
	      CXX="clang++-3.8"
	      config_opts="--with-llvm-config=llvm-config-3.8 CXX=$CXX"
    ;;
    llvm-3.9)
	      CXX="clang++-3.9"
	      config_opts="--with-llvm-config=llvm-config-3.9 CXX=$CXX"
    ;;
    llvm-4.0)
	      CXX="clang++-4.0"
	      config_opts="--with-llvm-config=llvm-config-4.0 CXX=$CXX"
    ;;
    llvm-5.0)
	      CXX="clang++-5.0"
	      config_opts="--with-llvm-config=llvm-config-5.0 CXX=$CXX"
    ;;
    llvm-6.0)
	      CXX="clang++-6.0"
	      config_opts="--with-llvm-config=llvm-config-6.0 CXX=$CXX"
    ;;
    *)
	      echo "$ANSI_RED[GHDL - build] Unknown build $BLD $ANSI_NOCOLOR"
	      exit 1;;
esac

if [ ! "$(echo $BLD | grep gcc)" ]; then
    echo "../configure --prefix=$prefix $config_opts"
    ../configure "--prefix=$prefix" $config_opts
fi

travis_finish "configure" -notime

#--- make

travis_start "make" "$ANSI_YELLOW[GHDL - build] Make $ANSI_NOCOLOR"
set +e
make -j$(nproc) 2>make_err.log
tail -1000 make_err.log
set -e
travis_finish "make"

travis_start "install" "$ANSI_YELLOW[GHDL - build] Install $ANSI_NOCOLOR"
make install
cd ..
travis_finish "install"

if [ "$(echo $BLD | grep gcc)" ]; then
    travis_start "make_ghdllib" "$ANSI_YELLOW[GHDL - build] Make ghdllib $ANSI_NOCOLOR"
    make ghdllib
    travis_finish "make_ghdllib"

    travis_start "install_ghdllib" "$ANSI_YELLOW[GHDL - build] Install ghdllib $ANSI_NOCOLOR"
    make install
    cd ..
    travis_finish "install_ghdllib"
fi

#--- package

travis_start "tar.bin" "$ANSI_YELLOW[GHDL - build] Create package ${ANSI_DARKCYAN}${PKG_NAME}.tgz $ANSI_NOCOLOR"
tar -zcvf "${PKG_NAME}.tgz" -C "$prefix" .
travis_finish "tar.bin"

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

echo "[SUCCESSFUL]"
touch build_ok
