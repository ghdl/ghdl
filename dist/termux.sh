# Build and install GHDL on termux (https://termux.com/)

cd $(dirname "$0")/..

curl -fsSL https://its-pointless.github.io/setup-pointless-repo.sh | bash -
pkg install gnat-10 llvm make
setupgcc-10

# Temporal fix. See https://github.com/its-pointless/gcc_termux/issues/100
ln -s $PREFIX/bin/gnatmake-10 $PREFIX/bin/gnatmake

mkdir -p build-termux
cd build-termux
../configure --default-pic --enable-synth --with-llvm-config=llvm-config --prefix="$PREFIX"
make
make install
