#!/usr/bin/env bash

cd $(dirname "$0")/../../..

gstart () {
  printf "\e[36m> $@\e[0m\n"
}
gend () {
  printf "$@\n"
}

if [ -n "$CI" ]; then
  gstart () {
    printf '::group::'
    printf "$@\n"
    SECONDS=0
  }

  gend () {
    duration=$SECONDS
    echo '::endgroup::'
    printf "$@\n"
  }
fi

for item in $(find ./ -type f -name "*.vhdl"); do
  gstart "$item"
  ./pyGHDL/cli/DOM.py $item \
  && gend '\e[32mpass\e[0m' \
  || gend '\e[31mfail\e[0m'
done
