#! /bin/sh

. ../../testenv.sh

synth leds.vhdl spin1.vhdl leds_wrapper.vhdl leds_wrapper_arch_entity_inst.vhdl -e > syn_leds.vhdl

echo "Test successful"
