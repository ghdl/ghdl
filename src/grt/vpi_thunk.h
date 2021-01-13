/* GHDL Run Time (GRT) - VPI thunks
   Copyright (C) 2003 - 2016 Tristan Gingold & Felix Bertram

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

   As a special exception, if other files instantiate generics from this
   unit, or you link this unit with other files to produce an executable,
   this unit does not by itself cause the resulting executable to be
   covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU Public License.
*/

/* VPI thunk is a GHDL structure used to interface between DLL/DSO and
   GRT.  This is in particular needed on Windows where undefined references
   in DLL are not supported.  */

#include "vpi_user.h"

typedef struct {
  vpiHandle  (*vpi_register_systf) (p_vpi_systf_data data);
  int        (*vpi_vprintf)        (char *fmt, va_list ap);
  PLI_UINT32 (*vpi_mcd_close)      (PLI_UINT32 mcd);
  char*      (*vpi_mcd_name)       (PLI_UINT32 mcd);
  PLI_UINT32 (*vpi_mcd_open)       (char *name);
  PLI_UINT32 (*vpi_mcd_open_x)     (char *name, char *mode);
  int        (*vpi_mcd_vprintf)    (PLI_UINT32 mcd, char *fmt, va_list ap);
  int        (*vpi_mcd_fputc)      (PLI_UINT32 mcd, unsigned char x);
  int        (*vpi_mcd_fgetc)      (PLI_UINT32 mcd);
  vpiHandle  (*vpi_register_cb)    (p_cb_data data);
  int        (*vpi_remove_cb)      (vpiHandle ref);
  void       (*vpi_sim_vcontrol)   (int operation, va_list ap);
  vpiHandle  (*vpi_handle)         (int type, vpiHandle ref);
  vpiHandle  (*vpi_iterate)        (int type, vpiHandle ref);
  vpiHandle  (*vpi_scan)           (vpiHandle iter);
  vpiHandle  (*vpi_handle_by_index)(vpiHandle ref, int index);
  void       (*vpi_get_time)       (vpiHandle obj, s_vpi_time*t);
  int        (*vpi_get)            (int property, vpiHandle ref);
  char*      (*vpi_get_str)        (int property, vpiHandle ref);
  void       (*vpi_get_value)      (vpiHandle expr, p_vpi_value value);
  vpiHandle  (*vpi_put_value)      (vpiHandle obj, p_vpi_value value,
				      p_vpi_time when, int flags);
  int        (*vpi_free_object)    (vpiHandle ref);
  int        (*vpi_get_vlog_info)  (p_vpi_vlog_info vlog_info_p);
  int        (*vpi_chk_error)      (p_vpi_error_info info);
  vpiHandle  (*vpi_handle_by_name) (char *name, vpiHandle scope);
  int        (*vpi_control_np)     (int op, int status);
} vpi_thunk, *p_vpi_thunk;
