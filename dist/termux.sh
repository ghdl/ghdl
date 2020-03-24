# Build and install GHDL on termux (https://termux.com/)

cd $(dirname "$0")/..

curl -fsSL https://its-pointless.github.io/setup-pointless-repo.sh | bash -
pkg install gnat-9
setupgcc-9

mkdir -p build-termux
cd build-termux
../configure --default-pic --enable-synth --with-llvm-config=llvm-config --prefix="$PREFIX"
make
make install
