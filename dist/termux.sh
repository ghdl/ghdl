# Build and install GHDL on termux (https://termux.com/)

cd $(dirname "$0")/..

curl -fsSL https://its-pointless.github.io/setup-pointless-repo.sh | bash -
pkg install gnat-10 llvm clang make
setupgcc-10
setup-patchforgcc

mkdir -p build-termux
cd build-termux
CXX=clang++
../configure --default-pic --enable-synth --with-llvm-config=llvm-config --prefix="$PREFIX"
make
make install
