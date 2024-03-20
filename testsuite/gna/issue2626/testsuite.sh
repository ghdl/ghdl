#! /bin/sh

. ../../testenv.sh

FILES="
src/Example_Design/duplicator.vhd
src/Example_Design/hexifier.vhd
src/Example_Design/serializer.vhd
src/Example_Design/merginator.vhd
src/SDCard/sdcard_globals.vhd
src/SDCard/sdcard_cmd.vhd
src/Example_Design/sdcard_cmd_logger.vhd
src/SDCard/sdcard_dat.vhd
src/SDCard/sdcard_ctrl.vhd
src/SDCard/sdcard_wrapper.vhd
sim/sdcard_sim.vhd
sim/tb_sdcard.vhd
"
export GHDL_STD_FLAGS=--std=08
analyze $FILES
elab_simulate tb_sdcard  --assert-level=error --stop-time=200us

clean

echo "Test successful"
