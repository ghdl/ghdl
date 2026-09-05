#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mydesign.vhdl
elab min_bidi

if c_compiler_is_available && ghdl_has_feature min_bidi vpi; then
  "$GHDL" --vpi-compile -v "$CC" -c vpi1.c
  "$GHDL" --vpi-link -v "$CC" -o vpi1.vpi vpi1.o

  add_vpi_path

  simulate min_bidi --vpi=./vpi1.vpi | tee min_bidi.out
  diff_nocr min_bidi.out min_bidi.ref

  rm -f vpi1.vpi vpi1.o min_bidi.out
fi
clean

echo "Test successful"
