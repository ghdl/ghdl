#! /bin/sh

. ../../testenv.sh

mkdir dir1 dir2 || true

cd dir1
analyze ../hello.vhdl
elab_simulate hello

cd ../dir2
elab_simulate --workdir=../dir1 hello

cd ..
rm -rf dir1 dir2

echo "Test successful"
