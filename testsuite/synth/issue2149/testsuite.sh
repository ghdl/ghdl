#! /bin/sh

. ../../testenv.sh

synth --out=verilog -gCONFIG="10111011" rom_test.vhdl -e > syn_rom_test.v

if grep '"' syn_rom_test.v; then
  echo "failed"
  exit 1;
fi

echo "Test successful"
