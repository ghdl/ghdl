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
# LIBSRC_DIR: path to the libraries sources
# LIBDST_DIR: path to the build/destination dir.
# GHDL
# GHDL_FLAGS
# LN
# CP
# MKDIR
# VHDLLIBS_COPY_OBJS: must be set to 'no' for jit

vhdl.libs.all: vhdl.libs.v87 vhdl.libs.v93 vhdl.libs.v08

vhdl.libs.v93: std.v93 ieee.v93 synopsys.v93 mentor.v93
vhdl.libs.v87: std.v87 ieee.v87 synopsys.v87
vhdl.libs.v08: std.v08 ieee.v08 synopsys.v08

ANALYZE=$(GHDL) -a $(GHDL_FLAGS)
ANALYZE_DEP=$(GHDL)

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
  synopsys/std_logic_unsigned.vhdl \
  synopsys/std_logic_signed.vhdl
SYNOPSYS_V_BSRCS := synopsys/std_logic_misc.vhdl \
  synopsys/std_logic_misc-body.vhdl
SYNOPSYS8793_BSRCS := synopsys/std_logic_textio.vhdl
MENTOR_BSRCS := mentor/std_logic_arith.vhdl mentor/std_logic_arith_body.vhdl
IEEE08_BSRCS := \
  ieee2008/std_logic_1164.vhdl ieee2008/std_logic_1164-body.vhdl \
  ieee2008/std_logic_textio.vhdl \
  ieee2008/math_real.vhdl ieee2008/math_real-body.vhdl \
  ieee2008/math_complex.vhdl ieee2008/math_complex-body.vhdl \
  ieee2008/numeric_bit.vhdl ieee2008/numeric_bit-body.vhdl \
  ieee2008/numeric_bit_unsigned.vhdl ieee2008/numeric_bit_unsigned-body.vhdl \
  ieee2008/numeric_std.vhdl ieee2008/numeric_std-body.vhdl \
  ieee2008/numeric_std_unsigned.vhdl ieee2008/numeric_std_unsigned-body.vhdl \
  ieee2008/fixed_float_types.vhdl \
  ieee2008/fixed_generic_pkg.vhdl ieee2008/fixed_generic_pkg-body.vhdl \
  ieee2008/fixed_pkg.vhdl \
  ieee2008/float_generic_pkg.vhdl ieee2008/float_generic_pkg-body.vhdl \
  ieee2008/float_pkg.vhdl \
  ieee2008/ieee_bit_context.vhdl ieee2008/ieee_std_context.vhdl

STD87_BSRCS := $(STD_SRCS:.vhdl=.v87)
STD93_BSRCS := $(STD_SRCS:.vhdl=.v93)
STD08_BSRCS := $(STD_SRCS:.vhdl=.v08) std/env.vhdl std/env_body.vhdl
IEEE87_BSRCS := $(IEEE_SRCS:.vhdl=.v87)
IEEE93_BSRCS := $(IEEE_SRCS:.vhdl=.v93) $(MATH_SRCS)
SYNOPSYS87_BSRCS := $(SYNOPSYS_BSRCS) $(SYNOPSYS_V_BSRCS) $(SYNOPSYS8793_BSRCS)
SYNOPSYS93_BSRCS := $(SYNOPSYS_BSRCS) $(SYNOPSYS_V_BSRCS) $(SYNOPSYS8793_BSRCS)
SYNOPSYS08_BSRCS := $(SYNOPSYS_BSRCS) $(SYNOPSYS_V_BSRCS:.vhdl=.v08)
MENTOR93_BSRCS := $(MENTOR_BSRCS)

.PREFIXES: .vhdl .v93 .v87 .v08

SED_V93:= sed -e '/--V87/s/^/  --/' \
 -e '/--START-V08/,/--END-V08/s/^/--/'

SED_V87:= sed -e '/--!V87/s/^/  --/' -e '/--START-!V87/,/--END-!V87/s/^/--/' \
 -e '/--START-V08/,/--END-V08/s/^/--/'

SED_V08:= sed -e '/--V87/s/^/  --/' -e '/--!V08/s/^/  --/' \
 -e '/--START-!V08/,/--END-!V08/s/^/--/'

LIB87_DIR:=$(LIBDST_DIR)/v87
STD87_DIR:=$(LIB87_DIR)/std
IEEE87_DIR:=$(LIB87_DIR)/ieee
SYN87_DIR:=$(LIB87_DIR)/synopsys

LIB93_DIR:=$(LIBDST_DIR)/v93
STD93_DIR:=$(LIB93_DIR)/std
IEEE93_DIR:=$(LIB93_DIR)/ieee
SYN93_DIR:=$(LIB93_DIR)/synopsys
MENTOR93_DIR:=$(LIB93_DIR)/mentor

LIB08_DIR:=$(LIBDST_DIR)/v08
STD08_DIR:=$(LIB08_DIR)/std
IEEE08_DIR:=$(LIB08_DIR)/ieee
SYN08_DIR:=$(LIB08_DIR)/synopsys

ANALYZE87:=$(ANALYZE) --std=87
ANALYZE93:=$(ANALYZE) --std=93
ANALYZE08:=$(ANALYZE) --std=08

STD87_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(STD87_BSRCS))
STD93_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(STD93_BSRCS))
STD08_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(STD08_BSRCS))

IEEE93_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(IEEE93_BSRCS))
IEEE87_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(IEEE87_BSRCS))
IEEE08_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(IEEE08_BSRCS))
SYNOPSYS87_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(SYNOPSYS87_BSRCS))
SYNOPSYS93_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(SYNOPSYS93_BSRCS))
SYNOPSYS08_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(SYNOPSYS08_BSRCS))
MENTOR93_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(MENTOR93_BSRCS))
VITAL95_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(VITAL95_BSRCS))
VITAL2000_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(VITAL2000_BSRCS))

### V87 ###############################################################

$(LIBDST_DIR)/src/std/%.v87: $(LIBSRC_DIR)/std/%.vhdl
	$(SED_V87) < $< > $@

std.v87: $(STD87_DIR)/std-obj87.cf

$(STD87_DIR)/std-obj87.cf: $(ANALYZE_DEP) $(STD87_SRCS)
	$(RM) -f $@
	cd $(STD87_DIR); \
	for i in $(STD87_BSRCS); do \
	  cmd="$(ANALYZE87) --bootstrap --work=std ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/ieee/%.v87: $(LIBSRC_DIR)/ieee/%.vhdl
	$(SED_V87) < $< > $@

$(LIBDST_DIR)/src/vital95/%.vhdl: $(LIBSRC_DIR)/vital95/%.vhdl
	$(CP) $< $@

ANALYZE_IEEE87=$(ANALYZE87) -P../std --work=ieee

ieee.v87: $(IEEE87_DIR)/ieee-obj87.cf

$(IEEE87_DIR)/ieee-obj87.cf: $(ANALYZE_DEP) $(IEEE87_SRCS) $(VITAL95_SRCS) $(STD87_DIR)/std-obj87.cf
	$(RM) -f $@
	cd $(IEEE87_DIR); \
	for i in $(IEEE87_BSRCS) $(VITAL95_BSRCS); do \
          cmd="$(ANALYZE_IEEE87) ../../src/$$i";\
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/synopsys/%.vhdl: $(LIBSRC_DIR)/synopsys/%.vhdl
	$(CP) $< $@

synopsys.v87: $(SYN87_DIR)/ieee-obj87.cf

$(SYN87_DIR)/ieee-obj87.cf: $(ANALYZE_DEP) $(SYNOPSYS87_SRCS) $(IEEE87_DIR)/ieee-obj87.cf
	$(RM) -f $@
	cd $(SYN87_DIR); \
	$(CP) ../ieee/ieee-obj87.cf . ; \
	test x$(VHDLLIBS_COPY_OBJS) = "xno" || \
	for i in $(IEEE_SRCS) $(VITAL95_SRCS); do \
	  b=`basename $$i .vhdl`; \
	  if [ -f ../ieee/$$b.o ]; then \
	    $(RM) -f $$b.o; $(LN) ../ieee/$$b.o $$b.o || exit 1; \
	  fi; \
	done; \
	for i in $(SYNOPSYS87_BSRCS); do \
          cmd="$(ANALYZE_IEEE87) ../../src/$$i";\
	  echo $$cmd; eval $$cmd || exit 1; \
	done

### V93 ###############################################################


$(LIBDST_DIR)/src/std/%.v93: $(LIBSRC_DIR)/std/%.vhdl
	$(SED_V93) < $< > $@

std.v93: $(STD93_DIR)/std-obj93.cf

$(STD93_DIR)/std-obj93.cf: $(ANALYZE_DEP) $(STD93_SRCS)
	$(RM) -rf $@
	cd $(STD93_DIR); \
	for i in $(STD93_BSRCS); do \
	  cmd="$(ANALYZE93) --bootstrap --work=std ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/ieee/%.v93: $(LIBSRC_DIR)/ieee/%.vhdl
	$(SED_V93) < $< > $@

$(LIBDST_DIR)/src/ieee/%.vhdl: $(LIBSRC_DIR)/ieee/%.vhdl
	$(CP) $< $@

$(LIBDST_DIR)/src/vital2000/%.vhdl: $(LIBSRC_DIR)/vital2000/%.vhdl
	$(CP) $< $@

ANALYZE_IEEE93=$(ANALYZE93) -P../std --work=ieee

ieee.v93: $(IEEE93_DIR)/ieee-obj93.cf

$(IEEE93_DIR)/ieee-obj93.cf: $(ANALYZE_DEP) $(IEEE93_SRCS) $(VITAL2000_SRCS) $(STD93_DIR)/std-obj93.cf
	$(RM) -f $@
	cd $(IEEE93_DIR); \
	for i in $(IEEE93_BSRCS) $(VITAL2000_BSRCS); do \
          cmd="$(ANALYZE_IEEE93) ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

synopsys.v93: $(SYN93_DIR)/ieee-obj93.cf

$(SYN93_DIR)/ieee-obj93.cf: $(ANALYZE_DEP) $(SYNOPSYS93_SRCS) $(IEEE93_DIR)/ieee-obj93.cf
	$(RM) -f $@
	cd $(SYN93_DIR); \
	$(CP) ../ieee/ieee-obj93.cf .; \
	test x$(VHDLLIBS_COPY_OBJS) = "xno" || \
	for i in $(IEEE_SRCS) $(MATH_SRCS) $(VITAL2000_SRCS); do \
	  b=`basename $$i .vhdl`; \
	  if [ -f ../ieee/$$b.o ]; then \
	    $(RM) -f $$b.o; $(LN) ../ieee/$$b.o $$b.o || exit 1; \
	  fi; \
	done; \
	for i in $(SYNOPSYS93_BSRCS); do \
          cmd="$(ANALYZE_IEEE93) ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/mentor/%.vhdl: $(LIBSRC_DIR)/mentor/%.vhdl
	$(CP) $< $@

mentor.v93: $(MENTOR93_DIR)/ieee-obj93.cf

$(MENTOR93_DIR)/ieee-obj93.cf: $(ANALYZE_DEP) $(MENTOR93_SRCS) $(IEEE93_DIR)/ieee-obj93.cf
	$(RM) -f $@
	cd $(MENTOR93_DIR); \
	$(CP) ../ieee/ieee-obj93.cf . ;\
	test x$(VHDLLIBS_COPY_OBJS) = "xno" || \
	for i in $(IEEE_SRCS) $(MATH_SRCS) $(VITAL2000_SRCS); do \
	  b=`basename $$i .vhdl`; \
	  if [ -f ../ieee/$$b.o ]; then \
	    $(RM) -f $$b.o; $(LN) ../ieee/$$b.o $$b.o || exit 1; \
	  fi; \
	done ; \
	for i in $(MENTOR93_BSRCS); do \
          cmd="$(ANALYZE_IEEE93) ../../src/$$i";\
	  echo $$cmd; eval $$cmd || exit 1; \
	done

### V08 ###############################################################

$(LIBDST_DIR)/src/std/%.v08: $(LIBSRC_DIR)/std/%.vhdl
	$(SED_V08) < $< > $@

$(LIBDST_DIR)/src/std/%.vhdl: $(LIBSRC_DIR)/std/%.vhdl
	$(CP) $< $@

std.v08: $(STD08_DIR)/std-obj08.cf

$(STD08_DIR)/std-obj08.cf: $(ANALYZE_DEP) $(STD08_SRCS)
	$(RM) -f $@
	cd $(STD08_DIR); \
	for i in $(STD08_BSRCS); do \
	  cmd="$(ANALYZE08) --bootstrap --work=std ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/ieee2008/%.vhdl: $(LIBSRC_DIR)/ieee2008/%.vhdl
	$(CP) $< $@

ANALYZE_IEEE08=$(ANALYZE08) -P../std --work=ieee
ANALYZE_VITAL08=$(ANALYZE08) -P../std --work=ieee -frelaxed-rules

ieee.v08: $(IEEE08_DIR)/ieee-obj08.cf

$(IEEE08_DIR)/ieee-obj08.cf: $(ANALYZE_DEP) $(IEEE08_SRCS) $(STD08_DIR)/std-obj08.cf
	$(RM) -f $@
	cd $(IEEE08_DIR); \
	for i in $(IEEE08_BSRCS); do \
          cmd="$(ANALYZE_IEEE08) ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done; \
	for i in $(VITAL2000_BSRCS); do \
          cmd="$(ANALYZE_VITAL08) ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/synopsys/%.v08: $(LIBSRC_DIR)/synopsys/%.vhdl
	$(SED_V08) < $< > $@

synopsys.v08: $(SYN08_DIR)/ieee-obj08.cf

$(SYN08_DIR)/ieee-obj08.cf: $(ANALYZE_DEP) $(SYNOPSYS08_SRCS) $(IEEE08_DIR)/ieee-obj08.cf
	$(RM) -f $@
	cd $(SYN08_DIR); \
	$(CP) ../ieee/ieee-obj08.cf .; \
	test x$(VHDLLIBS_COPY_OBJS) = "xno" || \
	for i in $(IEEE08_SRCS) $(VITAL2000_SRCS); do \
	  b=`basename $$i .vhdl`; \
	  if [ -f ../ieee/$$b.o ]; then \
	    $(RM) -f $$b.o; $(LN) ../ieee/$$b.o $$b.o || exit 1; \
	  fi; \
	done; \
	for i in $(SYNOPSYS08_BSRCS); do \
          cmd="$(ANALYZE_IEEE08) ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

##############################################################################

ANALYZE_STD=$(GHDL) --bootstrap-standard $(GHDL_FLAGS)

$(LIB93_DIR)/std/std_standard.o: $(GHDL) std.v93
	cd $(LIB93_DIR)/std; $(ANALYZE_STD) --std=93

$(LIB87_DIR)/std/std_standard.o: $(GHDL) std.v87
	cd $(LIB87_DIR)/std; $(ANALYZE_STD) --std=87

$(LIB08_DIR)/std/std_standard.o: $(GHDL) std.v08
	cd $(LIB08_DIR)/std; $(ANALYZE_STD) --std=08

libs.vhdl.standard: $(LIB93_DIR)/std/std_standard.o \
 $(LIB87_DIR)/std/std_standard.o \
 $(LIB08_DIR)/std/std_standard.o
