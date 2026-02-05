#! /bin/sh

. ../../testenv.sh

analyze rotate_byte.vhdl
analyze rotate_rl.vhdl
analyze rotate_rl_conf.vhdl
analyze top_level.vhdl
analyze top_level_conf.vhdl

synth top_level_conf  > syn_top_level_conf.vhdl

clean

echo "Test successful"
