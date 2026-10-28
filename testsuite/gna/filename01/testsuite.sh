#! /bin/sh

. ../../testenv.sh

if "$GHDL" --help | grep -q -e --link; then
  cp repro.vhdl './-Wl,@linker-args.vhdl'
  # Prefix the assembler input, but use the default workdir for linking.
  analyze --workdir=./ './-Wl,@linker-args.vhdl'
  "$GHDL" --bind response_option_poc
  "$GHDL" --list-link response_option_poc > link.txt

  grep -F -q -- '-Wl,@linker-args.o' link.txt
  if grep -F -x -q -- '-Wl,@linker-args.o' link.txt; then
    echo "option-like object filename was not disambiguated"
    exit 1
  fi

  rm -f -- './-Wl,@linker-args.vhdl' './-Wl,@linker-args.o' link.txt
  clean
fi

echo "Test successful"
