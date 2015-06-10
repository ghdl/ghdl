#! /bin/sh

. ../../testenv.sh

mkdir dir1 dir2 dir3 || true

cd dir1
analyze ../hello.vhdl
elab_simulate -v hello
elab -v -o ../dir3/hello1 hello

cd ../dir2
elab_simulate -v --workdir=../dir1 hello

cd ..
rm -rf dir1 dir2 dir3 dir4

echo "Test successful"
