#  -*- Makefile -*- for the VHDL libraries.
#  Copyright (C) 2002-2021 Tristan Gingold
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.

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

enable_gplcompat=false

vhdl.libs.v87: std.v87 ieee.v87
vhdl.libs.v93: std.v93 ieee.v93
vhdl.libs.v08: std.v08 ieee.v08 $(LIBDST_DIR)/src/ieee2008/LICENSE

STD_SRCS := \
  textio.vhdl \
  textio-body.vhdl

SYNOPSYS_BSRCS := \
  std_logic_arith.vhdl \
  std_logic_unsigned.vhdl \
  std_logic_signed.vhdl

SYNOPSYS_V_BSRCS := \
  std_logic_misc.vhdl \
  std_logic_misc-body.vhdl

SYNOPSYS8793_BSRCS := \
  std_logic_textio.vhdl

UPF_SRCS := \
  upf/upf.vhdl \
  upf/upf-body.vhdl

IEEE_SRCS := \
  std_logic_1164.vhdl \
  std_logic_1164-body.vhdl \
  numeric_bit.vhdl \
  numeric_bit-body.vhdl \
  numeric_std.vhdl \
  numeric_std-body.vhdl

MATH_SRCS := \
  math_real.vhdl \
  math_real-body.vhdl \
  math_complex.vhdl \
  math_complex-body.vhdl

IEEE08_BSRCS := \
  ieee2008/std_logic_1164.vhdl \
  ieee2008/std_logic_1164-body.vhdl \
  ieee2008/std_logic_textio.vhdl \
  ieee2008/math_real.vhdl \
  ieee2008/math_real-body.vhdl \
  ieee2008/math_complex.vhdl \
  ieee2008/math_complex-body.vhdl \
  ieee2008/numeric_bit.vhdl \
  ieee2008/numeric_bit-body.vhdl \
  ieee2008/numeric_bit_unsigned.vhdl \
  ieee2008/numeric_bit_unsigned-body.vhdl \
  ieee2008/numeric_std.vhdl \
  ieee2008/numeric_std-body.vhdl \
  ieee2008/numeric_std_unsigned.vhdl \
  ieee2008/numeric_std_unsigned-body.vhdl \
  ieee2008/fixed_float_types.vhdl \
  ieee2008/fixed_generic_pkg.vhdl \
  ieee2008/fixed_generic_pkg-body.vhdl \
  ieee2008/fixed_pkg.vhdl \
  ieee2008/float_generic_pkg.vhdl \
  ieee2008/float_generic_pkg-body.vhdl \
  ieee2008/float_pkg.vhdl \
  ieee2008/ieee_bit_context.vhdl \
  ieee2008/ieee_std_context.vhdl \
  $(UPF_SRCS)

IEEE87_BSRCS := $(addprefix ieee/v87/,$(IEEE_SRCS)) $(UPF_SRCS)
IEEE93_BSRCS := $(addprefix ieee/v93/,$(IEEE_SRCS)) $(addprefix ieee/,$(MATH_SRCS)) $(UPF_SRCS)

VITAL95_BSRCS :=
VITAL2000_BSRCS :=
ifeq ($(enable_gplcompat),false)
VITAL95_BSRCS := \
  vital95/vital_timing.vhdl \
  vital95/vital_timing-body.vhdl \
  vital95/vital_primitives.vhdl \
  vital95/vital_primitives-body.vhdl
VITAL2000_BSRCS := \
  vital2000/timing_p.vhdl \
  vital2000/timing_b.vhdl \
  vital2000/prmtvs_p.vhdl \
  vital2000/prmtvs_b.vhdl \
  vital2000/memory_p.vhdl \
  vital2000/memory_b.vhdl
endif

ANALYZE=$(GHDL) -a $(GHDL_FLAGS)
ANALYZE_DEP=$(GHDL)

STD87_BSRCS := $(addprefix std/v87/,$(STD_SRCS))
STD93_BSRCS := $(addprefix std/v93/,$(STD_SRCS))
STD08_BSRCS := $(addprefix std/v08/,$(STD_SRCS)) std/env.vhdl std/env-body.vhdl
SYNOPSYS87_BSRCS := $(addprefix synopsys/,$(SYNOPSYS_BSRCS) $(SYNOPSYS_V_BSRCS) $(SYNOPSYS8793_BSRCS))
SYNOPSYS93_BSRCS := $(addprefix synopsys/,$(SYNOPSYS_BSRCS) $(SYNOPSYS_V_BSRCS) $(SYNOPSYS8793_BSRCS))
SYNOPSYS08_BSRCS := $(addprefix synopsys/,$(SYNOPSYS_BSRCS)) $(addprefix synopsys/v08/,$(SYNOPSYS_V_BSRCS))

.PREFIXES: .vhdl

SED_V93:= sed -e '/--V87/s/^/  --/' \
 -e '/--START-V08/,/--END-V08/s/^/--/'

SED_V87:= sed -e '/--!V87/s/^/  --/' -e '/--START-!V87/,/--END-!V87/s/^/--/' \
 -e '/--START-V08/,/--END-V08/s/^/--/'

SED_V08:= sed -e '/--V87/s/^/  --/' -e '/--!V08/s/^/  --/' \
 -e '/--START-!V08/,/--END-!V08/s/^/--/'

STD87_DIR:=$(LIBDST_DIR)/std/v87
IEEE87_DIR:=$(LIBDST_DIR)/ieee/v87

STD93_DIR:=$(LIBDST_DIR)/std/v93
IEEE93_DIR:=$(LIBDST_DIR)/ieee/v93

STD08_DIR:=$(LIBDST_DIR)/std/v08
IEEE08_DIR:=$(LIBDST_DIR)/ieee/v08

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
VITAL95_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(VITAL95_BSRCS))
VITAL2000_SRCS=$(addprefix $(LIBDST_DIR)/src/,$(VITAL2000_BSRCS))

### V87 ###############################################################

$(LIBDST_DIR)/src/std/v87/%.vhdl: $(LIBSRC_DIR)/std/%.vhdl
	$(SED_V87) < $< > $@

std.v87: $(STD87_DIR)/std-obj87.cf

$(STD87_DIR)/std-obj87.cf: $(ANALYZE_DEP) $(STD87_SRCS)
	$(RM) -f $@
	cd $(STD87_DIR); \
	for i in $(STD87_BSRCS); do \
	  cmd="$(ANALYZE87) --bootstrap --work=std ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/ieee/v87/%.vhdl: $(LIBSRC_DIR)/ieee/%.vhdl
	$(SED_V87) < $< > $@

$(LIBDST_DIR)/src/upf/%.vhdl: $(LIBSRC_DIR)/upf/%.vhdl
	$(CP) $< $@

$(LIBDST_DIR)/src/vital95/%.vhdl: $(LIBSRC_DIR)/vital95/%.vhdl
	$(CP) $< $@

ANALYZE_IEEE87=$(ANALYZE87) -P../.. --work=ieee

ieee.v87: $(IEEE87_DIR)/ieee-obj87.cf

$(IEEE87_DIR)/ieee-obj87.cf: $(ANALYZE_DEP) $(IEEE87_SRCS) $(VITAL95_SRCS) $(SYNOPSYS87_SRCS) $(STD87_DIR)/std-obj87.cf
	echo dep: $(SYNOPSYS87_SRCS)
	$(RM) -f $@
	cd $(IEEE87_DIR); \
	for i in $(IEEE87_BSRCS) $(VITAL95_BSRCS); do \
          cmd="$(ANALYZE_IEEE87) ../../src/$$i";\
	  echo $$cmd; eval $$cmd || exit 1; \
	done; \
	for i in $(SYNOPSYS87_BSRCS); do \
          cmd="$(ANALYZE_IEEE87) -fsynopsys ../../src/$$i";\
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/synopsys/%.vhdl: $(LIBSRC_DIR)/synopsys/%.vhdl
	$(CP) $< $@

### V93 ###############################################################


$(LIBDST_DIR)/src/std/v93/%.vhdl: $(LIBSRC_DIR)/std/%.vhdl
	$(SED_V93) < $< > $@

std.v93: $(STD93_DIR)/std-obj93.cf

$(STD93_DIR)/std-obj93.cf: $(ANALYZE_DEP) $(STD93_SRCS)
	$(RM) -rf $@
	cd $(STD93_DIR); \
	for i in $(STD93_BSRCS); do \
	  cmd="$(ANALYZE93) --bootstrap --work=std ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/ieee/v93/%.vhdl: $(LIBSRC_DIR)/ieee/%.vhdl
	$(SED_V93) < $< > $@

$(LIBDST_DIR)/src/upf/%.vhdl: $(LIBSRC_DIR)/upf/%.vhdl
	$(CP) $< $@

$(LIBDST_DIR)/src/ieee/%.vhdl: $(LIBSRC_DIR)/ieee/%.vhdl
	$(CP) $< $@

$(LIBDST_DIR)/src/vital2000/%.vhdl: $(LIBSRC_DIR)/vital2000/%.vhdl
	$(CP) $< $@

ANALYZE_IEEE93=$(ANALYZE93) -P../.. --work=ieee

ieee.v93: $(IEEE93_DIR)/ieee-obj93.cf

$(IEEE93_DIR)/ieee-obj93.cf: $(ANALYZE_DEP) $(IEEE93_SRCS) $(VITAL2000_SRCS) $(SYNOPSYS93_SRCS) $(STD93_DIR)/std-obj93.cf
	$(RM) -f $@
	cd $(IEEE93_DIR); \
	for i in $(IEEE93_BSRCS) $(VITAL2000_BSRCS); do \
          cmd="$(ANALYZE_IEEE93) ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done; \
	for i in $(SYNOPSYS93_BSRCS); do \
          cmd="$(ANALYZE_IEEE93) -fsynopsys ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

### V08 ###############################################################

$(LIBDST_DIR)/src/std/v08/%.vhdl: $(LIBSRC_DIR)/std/%.vhdl
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

$(LIBDST_DIR)/src/ieee2008/LICENSE: $(LIBSRC_DIR)/ieee2008/LICENSE
	$(CP) $< $@

$(LIBDST_DIR)/src/upf/%.vhdl: $(LIBSRC_DIR)/upf/%.vhdl
	$(CP) $< $@


ANALYZE_IEEE08=$(ANALYZE08) -P../.. --work=ieee
ANALYZE_VITAL08=$(ANALYZE08) -P../.. --work=ieee -frelaxed-rules

ieee.v08: $(IEEE08_DIR)/ieee-obj08.cf

$(IEEE08_DIR)/ieee-obj08.cf: $(ANALYZE_DEP) $(IEEE08_SRCS) $(SYNOPSYS08_SRCS) $(STD08_DIR)/std-obj08.cf
	$(RM) -f $@
	cd $(IEEE08_DIR); \
	for i in $(IEEE08_BSRCS); do \
          cmd="$(ANALYZE_IEEE08) ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done; \
	for i in $(VITAL2000_BSRCS); do \
          cmd="$(ANALYZE_VITAL08) ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done; \
	for i in $(SYNOPSYS08_BSRCS); do \
          cmd="$(ANALYZE_IEEE08) -fsynopsys ../../src/$$i"; \
	  echo $$cmd; eval $$cmd || exit 1; \
	done

$(LIBDST_DIR)/src/synopsys/v08/%.vhdl: $(LIBSRC_DIR)/synopsys/%.vhdl
	$(SED_V08) < $< > $@

##############################################################################

ANALYZE_STD=$(GHDL) --bootstrap-standard $(GHDL_FLAGS)

$(STD87_DIR)/std_standard.o: $(GHDL) std.v87
	cd $(STD87_DIR); $(ANALYZE_STD) --std=87

$(STD93_DIR)/std_standard.o: $(GHDL) std.v93
	cd $(STD93_DIR); $(ANALYZE_STD) --std=93

$(STD08_DIR)/std_standard.o: $(GHDL) std.v08
	cd $(STD08_DIR); $(ANALYZE_STD) --std=08

libs.vhdl.standard: $(STD87_DIR)/std_standard.o \
 $(STD93_DIR)/std_standard.o \
 $(STD08_DIR)/std_standard.o
