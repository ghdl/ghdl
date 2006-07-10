#  -*- Makefile -*- for the VHDL libraries.
#  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
#
#  GHDL is free software; you can redistribute it and/or modify it under
#  the terms of the GNU General Public License as published by the Free
#  Software Foundation; either version 2, or (at your option) any later
#  version.
#
#  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#  for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with GCC; see the file COPYING.  If not, write to the Free
#  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#  02111-1307, USA.

# Variable to be defined:
# LIB93_DIR
# LIB87_DIR
# REL_DIR
# LIBSRC_DIR
# ANALYZE
# LN
# CP
# VHDLLIBS_COPY_OBJS
#
# Note: the source files are analyzed in the LIBxx_DIR.  So LIBSRC_DIR must be
# relative to the target directory.

STD_SRCS := std/textio.vhdl std/textio_body.vhdl
IEEE_SRCS := ieee/std_logic_1164.vhdl ieee/std_logic_1164_body.vhdl \
  ieee/numeric_bit.vhdl ieee/numeric_bit-body.vhdl \
  ieee/numeric_std.vhdl ieee/numeric_std-body.vhdl
MATH_SRCS := ieee/math_real.vhdl ieee/math_real-body.vhdl \
  ieee/math_complex.vhdl ieee/math_complex-body.vhdl
VITAL95_BSRCS := vital95/vital_timing.vhdl vital95/vital_timing_body.vhdl \
  vital95/vital_primitives.vhdl vital95/vital_primitives_body.vhdl
VITAL2000_BSRCS := vital2000/timing_p.vhdl vital2000/timing_b.vhdl \
  vital2000/prmtvs_p.vhdl vital2000/prmtvs_b.vhdl \
  vital2000/memory_p.vhdl vital2000/memory_b.vhdl
SYNOPSYS_BSRCS := synopsys/std_logic_arith.vhdl \
  synopsys/std_logic_textio.vhdl synopsys/std_logic_unsigned.vhdl \
  synopsys/std_logic_signed.vhdl \
  synopsys/std_logic_misc.vhdl synopsys/std_logic_misc-body.vhdl 
MENTOR_BSRCS := mentor/std_logic_arith.vhdl mentor/std_logic_arith_body.vhdl

STD87_BSRCS := $(STD_SRCS:.vhdl=.v87)
STD93_BSRCS := $(STD_SRCS:.vhdl=.v93)
IEEE87_BSRCS := $(IEEE_SRCS:.vhdl=.v87)
IEEE93_BSRCS := $(IEEE_SRCS:.vhdl=.v93) $(MATH_SRCS)
SYNOPSYS87_BSRCS := $(SYNOPSYS_BSRCS)
SYNOPSYS93_BSRCS := $(SYNOPSYS_BSRCS)
MENTOR93_BSRCS := $(MENTOR_BSRCS)

.PREFIXES: .vhdl .v93 .v87

%.v93: %.vhdl
	sed -e '/--V87/s/^/  --/' < $< > $@

%.v87: %.vhdl
	sed -e '/--V93/s/^/  --/' -e '/--START-V93/,/--END-V93/s/^/--/' \
	  < $< > $@

STD93_DIR:=$(LIB93_DIR)/std
IEEE93_DIR:=$(LIB93_DIR)/ieee
SYN93_DIR:=$(LIB93_DIR)/synopsys
MENTOR93_DIR:=$(LIB93_DIR)/mentor

STD87_DIR:=$(LIB87_DIR)/std
IEEE87_DIR:=$(LIB87_DIR)/ieee
SYN87_DIR:=$(LIB87_DIR)/synopsys

ANALYZE93:=$(ANALYZE) --std=93
ANALYZE87:=$(ANALYZE) --std=87

STD87_SRCS=$(addprefix $(LIBSRC_DIR)/,$(STD87_BSRCS))
STD93_SRCS=$(addprefix $(LIBSRC_DIR)/,$(STD93_BSRCS))
IEEE93_SRCS=$(addprefix $(LIBSRC_DIR)/,$(IEEE93_BSRCS))
IEEE87_SRCS=$(addprefix $(LIBSRC_DIR)/,$(IEEE87_BSRCS))
SYNOPSYS_SRCS=$(addprefix $(LIBSRC_DIR)/,$(SYNOPSYS_BSRCS))
MENTOR93_SRCS=$(addprefix $(LIBSRC_DIR)/,$(MENTOR93_BSRCS))
VITAL95_SRCS=$(addprefix $(LIBSRC_DIR)/,$(VITAL95_BSRCS))
VITAL2000_SRCS=$(addprefix $(LIBSRC_DIR)/,$(VITAL2000_BSRCS))

std.v93: $(LIB93_DIR) $(STD93_SRCS) force
	$(RM) -rf $(STD93_DIR)
	mkdir $(STD93_DIR)
	prev=`pwd`; cd $(STD93_DIR); \
	for i in $(STD93_SRCS); do \
	  echo $$i; \
	  $(ANALYZE93) --bootstrap --work=std $(REL_DIR)/$$i || exit 1; \
	done; \
	cd $$prev

ANALYZE_IEEE93=$(ANALYZE93) -P../std --work=ieee

ieee.v93: $(LIB93_DIR) $(IEEE93_SRCS) force
	$(RM) -rf $(IEEE93_DIR)
	mkdir $(IEEE93_DIR)
	prev=`pwd`; cd $(IEEE93_DIR); \
	for i in $(IEEE93_BSRCS) $(VITAL2000_BSRCS); do \
          cmd="$(ANALYZE_IEEE93) $(REL_DIR)/$(LIBSRC_DIR)/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done; \
	cd $$prev

synopsys.v93: $(LIB93_DIR) $(SYNOPSYS_SRCS) force
	$(RM) -rf $(SYN93_DIR)
	mkdir $(SYN93_DIR)
	prev=`pwd`; cd $(SYN93_DIR); \
	$(CP) ../ieee/ieee-obj93.cf .; \
	test x$(VHDLLIBS_COPY_OBJS) = "xno" || \
	for i in $(IEEE_SRCS) $(MATH_SRCS) $(VITAL2000_SRCS); do \
	  b=`basename $$i .vhdl`; $(LN) ../ieee/$$b.o $$b.o || exit 1; \
	done; \
	for i in $(SYNOPSYS93_BSRCS); do \
          cmd="$(ANALYZE_IEEE93) $(REL_DIR)/$(LIBSRC_DIR)/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done; \
	cd $$prev

mentor.v93: $(LIB93_DIR) $(MENTOR93_SRCS) force
	$(RM) -rf $(MENTOR93_DIR)
	mkdir $(MENTOR93_DIR)
	prev=`pwd`; cd $(MENTOR93_DIR); \
	$(CP) ../ieee/ieee-obj93.cf . ;\
	test x$(VHDLLIBS_COPY_OBJS) = "xno" || \
	for i in $(IEEE_SRCS) $(MATH_SRCS) $(VITAL2000_SRCS); do \
	  b=`basename $$i .vhdl`; $(LN) ../ieee/$$b.o $$b.o || exit 1; \
	done ; \
	for i in $(MENTOR93_BSRCS); do \
          cmd="$(ANALYZE_IEEE93) $(REL_DIR)/$(LIBSRC_DIR)/$$i";\
	  echo $$cmd; eval $$cmd || exit 1; \
	done

std.v87: $(LIB87_DIR) $(STD87_SRCS) force
	$(RM) -rf $(STD87_DIR)
	mkdir $(STD87_DIR)
	prev=`pwd`; cd $(STD87_DIR); \
	for i in $(STD87_SRCS); do \
	  echo $$i; \
	  $(ANALYZE87) --bootstrap --work=std $(REL_DIR)/$$i || exit 1; \
	done; \
	cd $$prev

ANALYZE_IEEE87=$(ANALYZE87) -P../std --work=ieee

ieee.v87: $(LIB87_DIR) $(IEEE87_SRCS) force
	$(RM) -rf $(IEEE87_DIR)
	mkdir $(IEEE87_DIR)
	prev=`pwd`; cd $(IEEE87_DIR); \
	for i in $(IEEE87_BSRCS) $(VITAL95_BSRCS); do \
          cmd="$(ANALYZE_IEEE87) $(REL_DIR)/$(LIBSRC_DIR)/$$i";\
	  echo $$cmd; eval $$cmd || exit 1; \
	done; \
	cd $$prev

synopsys.v87: $(LIB87_DIR) $(SYNOPSYS_SRCS) force
	$(RM) -rf $(SYN87_DIR)
	mkdir $(SYN87_DIR)
	prev=`pwd`; cd $(SYN87_DIR); \
	$(CP) ../ieee/ieee-obj87.cf . ; \
	test x$(VHDLLIBS_COPY_OBJS) = "xno" || \
	for i in $(IEEE_SRCS) $(VITAL95_SRCS); do \
	  b=`basename $$i .vhdl`; $(LN) ../ieee/$$b.o $$b.o || exit 1; \
	done; \
	for i in $(SYNOPSYS87_BSRCS); do \
          cmd="$(ANALYZE_IEEE87) $(REL_DIR)/$(LIBSRC_DIR)/$$i";\
	  echo $$cmd; eval $$cmd || exit 1; \
	done; \
	cd $$prev
