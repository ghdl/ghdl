#! /bin/bash

cd $(dirname $0)/..

rm -rf doc/_build/man/*

set -e

docker run --rm -it \
  -v /$(pwd):/src \
  -w //src/doc \
  btdi/sphinx:py2-featured \
  sh -c "sphinx-build -T -b man . ./_build/man"

nroff -man doc/_build/man/ghdl.1
