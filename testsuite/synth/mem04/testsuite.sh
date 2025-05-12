#! /bin/sh

. ../../testenv.sh

synth_tb rom_bit
grep -q -F '10 => "00001010",' syn_rom_bit.vhdl

synth_tb rom_ub32
# Check data is stored at the correct index
grep -q -F '3 => "00000011",' syn_rom_ub32.vhdl

synth_tb rom_log
# Check data is stored at the correct index
grep -q -F '3 => "00000011",' syn_rom_log.vhdl
grep -q -F '7 => "XXXX0111",' syn_rom_log.vhdl

synth_tb rom_sig_bit
grep -q -F '2 => "00000010",' syn_rom_sig_bit.vhdl

echo "Test successful"
