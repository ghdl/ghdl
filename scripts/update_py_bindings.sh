#!/usr/bin/env sh

set -e

cd $(dirname "$0")/../src

for item in edif psl vhdl; do
  cd "$item"
  make clean
  make
  cd ..
done
