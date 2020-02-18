# Build and install GHDL on termux (https://termux.com/)

cd $(dirname "$0")/..

curl -fsSL https://its-pointless.github.io/setup-pointless-repo.sh | bash -
pkg install gnat-9
setupgcc-9
ln -s $PREFIX/lib/gcc/aarch64-linux-android/9.2.0/libgnat-9.so $PREFIX/lib/libgnat-9.so
ln -s $PREFIX/lib/gcc/aarch64-linux-android/9.2.0/libgnarl-9.so $PREFIX/lib/libgnarl-9.so

mkdir -p build-termux
cd build-termux
../configure --default-pic --enable-synth --with-llvm-config=llvm-config --prefix="$PREFIX"
make
make install
