#! /bin/sh

. ../../testenv.sh

synth edge_enhance.vhd hdmi_design.vhd line_delay.vhd pixel_processing.vhd -e > syn_hdmi_design.vhdl

echo "Test successful"
