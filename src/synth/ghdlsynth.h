/*  Ghdlsynth -*- C++ -*- interface

   This file is part of GHDL.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

namespace GhdlSynth {
  //  Use struct wrappers for type safety.
#define GHDLSYNTH_ADA_PREFIX(N) netlists__##N
#define GHDLSYNTH_ADA_WRAPPER_WD(NAME, RESTYPE, ARGTYPE) \
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
  extern "C" unsigned int GHDLSYNTH_ADA_PREFIX(NAME) (unsigned int, ARGTYPE2);\
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

  struct Name_Id { unsigned int id; };
  extern "C" const char *name_table__get_address (unsigned int);
  inline const char *get_cstr(Name_Id n) {
    return name_table__get_address (n.id);
  }

  struct Sname { unsigned int id; };
  const Sname No_Sname = {0 };

  enum Sname_Kind { Sname_User, Sname_Artificial, Sname_Version };
  GHDLSYNTH_ADA_WRAPPER_DW(get_sname_kind, Sname_Kind, Sname);
  inline bool is_valid(Sname l) { return l.id != 0; }

  GHDLSYNTH_ADA_WRAPPER_WD(get_sname_prefix, Sname, Sname);
  GHDLSYNTH_ADA_WRAPPER_WD(get_sname_suffix, Name_Id, Sname);

  GHDLSYNTH_ADA_WRAPPER_DW(get_sname_version, unsigned int, Sname);

  typedef unsigned int Width;
  typedef unsigned int Port_Idx;
  typedef unsigned int Param_Idx;

#include "ghdlsynth_gates.h"

  struct Module { unsigned int id; };
  inline bool is_valid(Module m) { return m.id != 0; }
  GHDLSYNTH_ADA_WRAPPER_WD(get_module_name, Sname, Module);
  GHDLSYNTH_ADA_WRAPPER_WD(get_first_sub_module, Module, Module);
  GHDLSYNTH_ADA_WRAPPER_WD(get_next_sub_module, Module, Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_id, Module_Id, Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_nbr_outputs, unsigned int, Module);
  GHDLSYNTH_ADA_WRAPPER_DW(get_nbr_inputs, unsigned int, Module);

  struct Net { unsigned int id; };
  GHDLSYNTH_ADA_WRAPPER_DW(get_width, Width, Net);

  struct Instance { unsigned int id; };
  inline bool is_valid(Instance inst) { return inst.id != 0; }
  GHDLSYNTH_ADA_WRAPPER_WD(get_self_instance, Instance, Module);
  GHDLSYNTH_ADA_WRAPPER_WD(get_first_instance, Instance, Module);
  GHDLSYNTH_ADA_WRAPPER_WD(get_next_instance, Instance, Instance);
  GHDLSYNTH_ADA_WRAPPER_WD(get_instance_name, Sname, Instance);
  GHDLSYNTH_ADA_WRAPPER_WD(get_module, Module, Instance);
  GHDLSYNTH_ADA_WRAPPER_WD(get_net_parent, Instance, Net);
  GHDLSYNTH_ADA_WRAPPER_DWD(get_param_uns32, unsigned int, Instance, Port_Idx);

  struct Input { unsigned int id; };
  GHDLSYNTH_ADA_WRAPPER_WWD(get_input, Input, Instance, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_output, Net, Instance, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WD(get_driver, Net, Input);

  //  Utils
#undef GHDLSYNTH_ADA_PREFIX
#define GHDLSYNTH_ADA_PREFIX(N) netlists__utils__##N
  GHDLSYNTH_ADA_WRAPPER_DW(get_id, Module_Id, Instance);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_input_name, Sname, Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_WWD(get_output_name, Sname, Module, Port_Idx);
  GHDLSYNTH_ADA_WRAPPER_BW(has_one_connection, Net);

  extern "C" unsigned int libghdlsynth__synth(int argc, const char **argv);
  inline Module ghdl_synth(int argc, const char **argv) {
    Module res;
    res.id = libghdlsynth__synth(argc, argv);
    return res;
  }

  //  Disp ghdl configuration.
  extern "C" void ghdlcomp__disp_config (void);

  // Initialize and finalize the whole library.
  extern "C" void libghdlsynth_init (void);
  extern "C" void libghdlsynth_final (void);
};
