/*  Ghdlsynth -*- C -*- interface

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
#ifndef GHDL_SYNTH_H_
#define GHDL_SYNTH_H_

#ifdef __cplusplus
extern "C" {
#else
#include <stdbool.h>
#endif

  struct logic_32 {
    unsigned int va;
    unsigned int zx;
  };

  //  Use struct wrappers for type safety.
  //  Convention: W for wrapped, D for direct, B for boolean.
#define GHDLSYNTH_ADA_PREFIX(N) netlists__##N
#define GHDLSYNTH_ADA_WRAPPER_WW(NAME, RESTYPE, ARGTYPE) \
  unsigned int GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int); \
  inline RESTYPE NAME(ARGTYPE arg) { \
    RESTYPE res; \
    res.id = GHDLSYNTH_ADA_PREFIX(NAME) (arg.id);	\
    return res; \
  }

#define GHDLSYNTH_ADA_WRAPPER_WWD(NAME, RESTYPE, ARGTYPE1, ARGTYPE2)	\
  unsigned int GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int, ARGTYPE2);\
  inline RESTYPE NAME(ARGTYPE1 arg1, ARGTYPE2 arg2) {	\
    RESTYPE res; \
    res.id = GHDLSYNTH_ADA_PREFIX(NAME) (arg1.id, arg2);	\
    return res; \
  }

#define GHDLSYNTH_ADA_WRAPPER_DWD(NAME, RESTYPE, ARGTYPE1, ARGTYPE2)	\
  RESTYPE GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int, ARGTYPE2);\
  inline RESTYPE NAME(ARGTYPE1 arg1, ARGTYPE2 arg2) {	\
    return GHDLSYNTH_ADA_PREFIX(NAME) (arg1.id, arg2);	\
  }

#define GHDLSYNTH_ADA_WRAPPER_DW(NAME, RESTYPE, ARGTYPE)	\
  RESTYPE GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int); \
  inline RESTYPE NAME(ARGTYPE arg) { \
    return GHDLSYNTH_ADA_PREFIX(NAME) (arg.id); \
  }

#define GHDLSYNTH_ADA_WRAPPER_BW(NAME, ARGTYPE)	\
  unsigned int GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int); \
  inline bool NAME(ARGTYPE arg) { \
    return (GHDLSYNTH_ADA_PREFIX(NAME) (arg.id) & 1);	\
  }

#define GHDLSYNTH_ADA_WRAPPER_BWD(NAME, ARGTYPE1, ARGTYPE2)		\
  unsigned int GHDLSYNTH_ADA_PREFIX(NAME) (unsigned, unsigned); \
  inline bool NAME(ARGTYPE1 arg1, ARGTYPE2 arg2) {			\
    return (GHDLSYNTH_ADA_PREFIX(NAME) (arg1.id, arg2) & 1);		\
  }

  struct Name_Id { unsigned int id; };
  const char *name_table__get_address (unsigned int);
  inline const char *get_cstr(struct Name_Id n) {
    return name_table__get_address (n.id);
  }

  unsigned name_table__get_identifier_with_len(const char *s, unsigned l);
  inline struct Name_Id get_identifier(const char *s) {
    struct Name_Id n;
    n.id = name_table__get_identifier_with_len(s, strlen(s));
    return n;
  }

  struct Sname { unsigned int id; };
  const struct Sname No_Sname = {0 };

  enum Sname_Kind { Sname_User, Sname_Artificial, Sname_Version };
  GHDLSYNTH_ADA_WRAPPER_DW(get_sname_kind, enum Sname_Kind, struct Sname);
  inline bool sname_is_valid(struct Sname l) { return l.id != 0; }

  GHDLSYNTH_ADA_WRAPPER_WW(get_sname_prefix, struct Sname, struct Sname);
  GHDLSYNTH_ADA_WRAPPER_WW(get_sname_suffix, struct Name_Id, struct Sname);

  GHDLSYNTH_ADA_WRAPPER_DW(get_sname_version, unsigned int, struct Sname);

  typedef unsigned int Width;
  typedef unsigned int Port_Idx;
  typedef unsigned int Param_Idx;
  struct Pval { unsigned int id; };

#include "ghdl/synth_gates.h"

  struct Module { unsigned int id; };
  inline bool module_is_valid(struct Module m) { return m.id != 0; }
  GHDLSYNTH_ADA_WRAPPER_WW(get_module_name, struct Sname, struct Module);
  GHDLSYNTH_ADA_WRAPPER_WW(get_first_sub_module, struct Module, struct Module);
  GHDLSYNTH_ADA_WRAPPER_WW(get_next_sub_module, struct Module, struct Module);
  GHDLSYNTH_ADA_WRAPPER_DW(module_get_id, enum Module_Id, struct Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_nbr_outputs, unsigned int, struct Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_nbr_inputs, unsigned int, struct Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_nbr_params, unsigned int, struct Module);

  struct Net { unsigned int id; };
  GHDLSYNTH_ADA_WRAPPER_DW(get_width, Width, struct Net);

  struct Instance { unsigned int id; };
  inline bool instance_is_valid(struct Instance inst) { return inst.id != 0; }
  GHDLSYNTH_ADA_WRAPPER_WW(get_self_instance, struct Instance, struct Module);
  GHDLSYNTH_ADA_WRAPPER_WW(get_first_instance, struct Instance, struct Module);
  GHDLSYNTH_ADA_WRAPPER_WW(get_next_instance, struct Instance, struct Instance);
  GHDLSYNTH_ADA_WRAPPER_WW(get_instance_name, struct Sname, struct Instance);
  GHDLSYNTH_ADA_WRAPPER_WW(get_module, struct Module, struct Instance);
  GHDLSYNTH_ADA_WRAPPER_WW(get_net_parent, struct Instance, struct Net);
  GHDLSYNTH_ADA_WRAPPER_DWD(get_param_uns32, unsigned int, struct Instance, Param_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_param_pval, struct Pval, struct Instance, Param_Idx);
  GHDLSYNTH_ADA_WRAPPER_DW(get_pval_length, unsigned int, struct Pval);
  GHDLSYNTH_ADA_WRAPPER_DWD(read_pval, struct logic_32, struct Pval, unsigned int);

  struct Input { unsigned int id; };
  GHDLSYNTH_ADA_WRAPPER_WWD(get_input, struct Input, struct Instance, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_output, struct Net, struct Instance, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WW(get_driver, struct Net, struct Input);
  GHDLSYNTH_ADA_WRAPPER_WW(get_input_parent, struct Instance, struct Input);

  GHDLSYNTH_ADA_WRAPPER_WW(get_first_sink, struct Input, struct Net);
  GHDLSYNTH_ADA_WRAPPER_WW(get_next_sink, struct Input, struct Input);

  struct Attribute { unsigned int id; };
  GHDLSYNTH_ADA_WRAPPER_WW(get_first_attribute, struct Attribute, struct Instance);
  GHDLSYNTH_ADA_WRAPPER_WW(get_attribute_name, struct Name_Id, struct Attribute);
  GHDLSYNTH_ADA_WRAPPER_DW(get_attribute_type, enum Param_Type, struct Attribute);
  GHDLSYNTH_ADA_WRAPPER_WW(get_attribute_pval, struct Pval, struct Attribute);
  GHDLSYNTH_ADA_WRAPPER_WW(get_attribute_next, struct Attribute, struct Attribute);

  //  Utils
#undef GHDLSYNTH_ADA_PREFIX
#define GHDLSYNTH_ADA_PREFIX(N) netlists__utils__##N
  GHDLSYNTH_ADA_WRAPPER_DW(instance_get_id, enum Module_Id, struct Instance);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_input_name, struct Sname, struct Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_output_name, struct Sname, struct Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_DWD(get_input_width, Width, struct Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_DWD(get_output_width, Width, struct Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_BWD(get_inout_flag, struct Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_param_name, struct Sname, struct Module, Param_Idx);
  GHDLSYNTH_ADA_WRAPPER_DWD(get_param_type, enum Param_Type, struct Module, Param_Idx);
  GHDLSYNTH_ADA_WRAPPER_BW(has_one_connection, struct Net);

  GHDLSYNTH_ADA_WRAPPER_WWD(get_input_net, struct Net, struct Instance, Port_Idx);


  unsigned int ghdlsynth__ghdl_synth(int init,
                                     int argc, const char **argv);
  inline struct Module ghdl_synth(int init, int argc, const char **argv) {
    struct Module res;
    res.id = ghdlsynth__ghdl_synth(init, argc, argv);
    return res;
  }

  //  Disp ghdl configuration.
  void ghdlcomp__disp_config (void);

  // Initialize the whole library.
  void libghdl_init (void);

  // More initialization for synthesis.
  void ghdlsynth__init_for_ghdl_synth (void);

#ifdef __cplusplus
}
#endif

#endif /* GHDL_SYNTH_H_ */
