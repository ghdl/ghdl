#! /bin/sh

. ../../testenv.sh

#export GHDL_STD_FLAGS=--std=08
analyze main.vhdl
elab_simulate HA_Entity
elab_simulate HA_Config
analyze my_top.vhdl
elab_simulate ha_config
if elab_simulate ha_config my_top; then
  echo "failure expected"
  exit 1;
fi


clean

echo "Test successful"
