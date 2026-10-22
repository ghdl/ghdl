#! /bin/sh

. ../../testenv.sh

synth --out=verilog -gCONFIG="10111011" rom_test.vhdl -e > syn_rom_test.v

if sed -e 's/(\* src="[^"]*" \*) //g' syn_rom_test.v | grep '"'; then
  echo "failed"
  exit 1;
fi

echo "Test successful"
