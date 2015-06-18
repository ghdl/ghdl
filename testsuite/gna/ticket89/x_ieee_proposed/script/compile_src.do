#========================================================================================================================
# Copyright (c) 2015 by Bitvis AS.  All rights reserved.
#
# BITVIS UTILITY LIBRARY AND ANY PART THEREOF ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH BITVIS UTILITY LIBRARY.
#========================================================================================================================

# This file may be called with an argument
# arg 1: Part directory of this library/module

if {[batch_mode]} {
  onerror {abort all; exit -f -code 1}
} else {
  onerror {abort all}
}

# Set up part_path and lib_name
#------------------------------------------------------
quietly set lib_name "ieee_proposed"
quietly set part_name "x_ieee_proposed"
# path from mpf-file in sim
quietly set part_path "../..//$part_name"

if { [info exists 1] } {
  # path from this part to target part
  quietly set part_path "$1/..//$part_name"
  unset 1
}



if {[file exists $part_path/sim/$lib_name]} {
  file delete -force $part_path/sim/$lib_name
}
if {![file exists $part_path/sim]} {
  file mkdir $part_path/sim
}

vlib $part_path/sim/$lib_name
vmap $lib_name $part_path/sim/$lib_name


echo "\n\n\n=== Compiling $lib_name source\n"
vcom -93 -work $lib_name $part_path/src/standard_additions_c.vhdl
vcom -93 -work $lib_name $part_path/src/standard_textio_additions_c.vhdl
vcom -93 -work $lib_name $part_path/src/std_logic_1164_additions.vhdl
vcom -93 -work $lib_name $part_path/src/numeric_std_additions.vhdl

