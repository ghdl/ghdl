###############################################################################
# Copyright (c) 2015 Potential Ventures Ltd
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Potential Ventures Ltd,
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL POTENTIAL VENTURES LTD BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
###############################################################################

TOPLEVEL_LANG ?= vhdl

ifneq ($(TOPLEVEL_LANG),vhdl)

all:
	@echo "Skipping test due to TOPLEVEL_LANG=$(TOPLEVEL_LANG) not being vhdl"
clean::

else

TOPLEVEL = dec_viterbi_top

ifeq ($(OS),Msys)
WPWD=$(shell sh -c 'pwd -W')
else
WPWD=$(shell pwd)
endif

COCOTB?=$(WPWD)/../../..

SRC_BASE = $(COCOTB)/tests/designs/viterbi_decoder_axi4s

RTL_LIBRARY = dec_viterbi

VHDL_SOURCES = $(SRC_BASE)/packages/pkg_helper.vhd \
	       $(SRC_BASE)/packages/pkg_param.vhd \
	       $(SRC_BASE)/packages/pkg_param_derived.vhd \
	       $(SRC_BASE)/packages/pkg_types.vhd \
	       $(SRC_BASE)/packages/pkg_components.vhd \
	       $(SRC_BASE)/packages/pkg_trellis.vhd \
	       $(SRC_BASE)/src/generic_sp_ram.vhd \
	       $(SRC_BASE)/src/axi4s_buffer.vhd \
	       $(SRC_BASE)/src/branch_distance.vhd \
	       $(SRC_BASE)/src/traceback.vhd \
	       $(SRC_BASE)/src/acs.vhd \
	       $(SRC_BASE)/src/ram_ctrl.vhd \
	       $(SRC_BASE)/src/reorder.vhd \
	       $(SRC_BASE)/src/recursion.vhd \
	       $(SRC_BASE)/src/dec_viterbi.vhd \

include $(COCOTB)/makefiles/Makefile.inc
include $(COCOTB)/makefiles/Makefile.sim

endif
