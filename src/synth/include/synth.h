/*  Ghdlsynth -*- C++ -*- interface

  This file is part of GHDL.

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <gnu.org/licenses>.
*/

namespace GhdlSynth {
  struct logic_32 {
    unsigned int va;
    unsigned int zx;
  };

  //  Use struct wrappers for type safety.
  //  Convention: W for wrapped, D for direct, B for boolean.
#define GHDLSYNTH_ADA_PREFIX(N) netlists__##N
#define GHDLSYNTH_ADA_WRAPPER_WW(NAME, RESTYPE, ARGTYPE) \
  extern "C" unsigned int GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int); \
  inline RESTYPE NAME(ARGTYPE arg) { \
    RESTYPE res; \
    res.id = GHDLSYNTH_ADA_PREFIX(NAME) (arg.id);	\
    return res; \
  }

#define GHDLSYNTH_ADA_WRAPPER_WWD(NAME, RESTYPE, ARGTYPE1, ARGTYPE2)	\
  extern "C" unsigned int GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int, ARGTYPE2);\
  inline RESTYPE NAME(ARGTYPE1 arg1, ARGTYPE2 arg2) {	\
    RESTYPE res; \
    res.id = GHDLSYNTH_ADA_PREFIX(NAME) (arg1.id, arg2);	\
    return res; \
  }

#define GHDLSYNTH_ADA_WRAPPER_DWD(NAME, RESTYPE, ARGTYPE1, ARGTYPE2)	\
  extern "C" RESTYPE GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int, ARGTYPE2);\
  inline RESTYPE NAME(ARGTYPE1 arg1, ARGTYPE2 arg2) {	\
    return GHDLSYNTH_ADA_PREFIX(NAME) (arg1.id, arg2);	\
  }

#define GHDLSYNTH_ADA_WRAPPER_DW(NAME, RESTYPE, ARGTYPE)	\
  extern "C" RESTYPE GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int); \
  inline RESTYPE NAME(ARGTYPE arg) { \
    return GHDLSYNTH_ADA_PREFIX(NAME) (arg.id); \
  }

#define GHDLSYNTH_ADA_WRAPPER_BW(NAME, ARGTYPE)	\
  extern "C" unsigned int GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int); \
  inline bool NAME(ARGTYPE arg) { \
    return (GHDLSYNTH_ADA_PREFIX(NAME) (arg.id) & 1);	\
  }

#define GHDLSYNTH_ADA_WRAPPER_BWD(NAME, ARGTYPE1, ARGTYPE2)		\
  extern "C" unsigned int GHDLSYNTH_ADA_PREFIX(NAME) (unsigned, unsigned); \
  inline bool NAME(ARGTYPE1 arg1, ARGTYPE2 arg2) {			\
    return (GHDLSYNTH_ADA_PREFIX(NAME) (arg1.id, arg2) & 1);		\
  }

  struct Name_Id { unsigned int id; };
  extern "C" const char *name_table__get_address (unsigned int);
  inline const char *get_cstr(Name_Id n) {
    return name_table__get_address (n.id);
  }

  extern "C" unsigned name_table__get_identifier_with_len(const char *s, unsigned l);
  inline Name_Id get_identifier(const char *s) {
    Name_Id n;
    n.id = name_table__get_identifier_with_len(s, strlen(s));
    return n;
  }

  struct Sname { unsigned int id; };
  const Sname No_Sname = {0 };

  enum Sname_Kind { Sname_User, Sname_Artificial, Sname_Version };
  GHDLSYNTH_ADA_WRAPPER_DW(get_sname_kind, Sname_Kind, Sname);
  inline bool is_valid(Sname l) { return l.id != 0; }

  GHDLSYNTH_ADA_WRAPPER_WW(get_sname_prefix, Sname, Sname);
  GHDLSYNTH_ADA_WRAPPER_WW(get_sname_suffix, Name_Id, Sname);

  GHDLSYNTH_ADA_WRAPPER_DW(get_sname_version, unsigned int, Sname);

  typedef unsigned int Width;
  typedef unsigned int Port_Idx;
  typedef unsigned int Param_Idx;
  struct Pval { unsigned int id; };

#include "ghdl/synth_gates.h"

  struct Module { unsigned int id; };
  inline bool is_valid(Module m) { return m.id != 0; }
  GHDLSYNTH_ADA_WRAPPER_WW(get_module_name, Sname, Module);
  GHDLSYNTH_ADA_WRAPPER_WW(get_first_sub_module, Module, Module);
  GHDLSYNTH_ADA_WRAPPER_WW(get_next_sub_module, Module, Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_id, Module_Id, Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_nbr_outputs, unsigned int, Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_nbr_inputs, unsigned int, Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_nbr_params, unsigned int, Module);

  struct Net { unsigned int id; };
  GHDLSYNTH_ADA_WRAPPER_DW(get_width, Width, Net);

  struct Instance { unsigned int id; };
  inline bool is_valid(Instance inst) { return inst.id != 0; }
  GHDLSYNTH_ADA_WRAPPER_WW(get_self_instance, Instance, Module);
  GHDLSYNTH_ADA_WRAPPER_WW(get_first_instance, Instance, Module);
  GHDLSYNTH_ADA_WRAPPER_WW(get_next_instance, Instance, Instance);
  GHDLSYNTH_ADA_WRAPPER_WW(get_instance_name, Sname, Instance);
  GHDLSYNTH_ADA_WRAPPER_WW(get_module, Module, Instance);
  GHDLSYNTH_ADA_WRAPPER_WW(get_net_parent, Instance, Net);
  GHDLSYNTH_ADA_WRAPPER_DWD(get_param_uns32, unsigned int, Instance, Param_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_param_pval, Pval, Instance, Param_Idx);
  GHDLSYNTH_ADA_WRAPPER_DW(get_pval_length, unsigned int, Pval);
  GHDLSYNTH_ADA_WRAPPER_DWD(read_pval, struct logic_32, Pval, unsigned int);

  struct Input { unsigned int id; };
  GHDLSYNTH_ADA_WRAPPER_WWD(get_input, Input, Instance, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_output, Net, Instance, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WW(get_driver, Net, Input);
  GHDLSYNTH_ADA_WRAPPER_WW(get_input_parent, Instance, Input);

  GHDLSYNTH_ADA_WRAPPER_WW(get_first_sink, Input, Net);
  GHDLSYNTH_ADA_WRAPPER_WW(get_next_sink, Input, Input);

  struct Attribute { unsigned int id; };
  GHDLSYNTH_ADA_WRAPPER_WW(get_instance_first_attribute, Attribute, Instance);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_input_port_first_attribute, Attribute, Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_output_port_first_attribute, Attribute, Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WW(get_attribute_name, Name_Id, Attribute);
  GHDLSYNTH_ADA_WRAPPER_DW(get_attribute_type, Param_Type, Attribute);
  GHDLSYNTH_ADA_WRAPPER_WW(get_attribute_pval, Pval, Attribute);
  GHDLSYNTH_ADA_WRAPPER_WW(get_attribute_next, Attribute, Attribute);

  //  Utils
#undef GHDLSYNTH_ADA_PREFIX
#define GHDLSYNTH_ADA_PREFIX(N) netlists__utils__##N
  GHDLSYNTH_ADA_WRAPPER_DW(get_id, Module_Id, Instance);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_input_name, Sname, Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_output_name, Sname, Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_DWD(get_input_width, Width, Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_DWD(get_output_width, Width, Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_BWD(get_inout_flag, Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_param_name, Sname, Module, Param_Idx);
  GHDLSYNTH_ADA_WRAPPER_DWD(get_param_type, Param_Type, Module, Param_Idx);
  GHDLSYNTH_ADA_WRAPPER_BW(has_one_connection, Net);

  GHDLSYNTH_ADA_WRAPPER_WWD(get_input_net, Net, Instance, Port_Idx);


  extern "C" unsigned int ghdlsynth__ghdl_synth(int init,
                                                int argc, const char **argv);
  inline Module ghdl_synth(int init, int argc, const char **argv) {
    Module res;
    res.id = ghdlsynth__ghdl_synth(init, argc, argv);
    return res;
  }

  //  Disp ghdl configuration.
  extern "C" void ghdlcomp__disp_config (void);

  // Initialize the whole library.
  extern "C" void libghdl_init (void);

  // More initialization for synthesis.
  extern "C" void ghdlsynth__init_for_ghdl_synth (void);
};
