/* GHDL Run Time (GRT) - VPI thunks
   Copyright (C) 2016 Tristan Gingold & Patrick Lehmann

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
#include <stdarg.h>

/* Define PLI_PROTOTYPES so that vpi_user.h do not import functions.  */
#define PLI_PROTOTYPES
#define PROTO_PARAMS(params) params
#define XXTERN extern

#include "vpi_thunk.h"

// the function pointer table
vpi_thunk *VPI_THUNK;

// stub functions for VPI
vpiHandle
vpi_register_systf (s_vpi_systf_data *s)
{
  return VPI_THUNK->vpi_register_systf (s);
}

int
vpi_vprintf (char *fmt, va_list ap)
{
  return VPI_THUNK->vpi_vprintf (fmt, ap);
}

unsigned int
vpi_mcd_close (unsigned int mcd)
{
  return VPI_THUNK->vpi_mcd_close (mcd);
}

char *
vpi_mcd_name (unsigned int mcd)
{
  return VPI_THUNK->vpi_mcd_name (mcd);
}

unsigned int
vpi_mcd_open (char *name)
{
  return VPI_THUNK->vpi_mcd_open (name);
}

unsigned int
vpi_mcd_open_x (char *name, char *mode)
{
  return VPI_THUNK->vpi_mcd_open_x (name, mode);
}

int
vpi_mcd_vprintf (unsigned int mcd, char *fmt, va_list ap)
{
  return VPI_THUNK->vpi_mcd_vprintf (mcd, fmt, ap);
}

int
vpi_mcd_fputc (unsigned int mcd, unsigned char x)
{
  return VPI_THUNK->vpi_mcd_fputc (mcd, x);
}

int
vpi_mcd_fgetc (unsigned int mcd)
{
  return VPI_THUNK->vpi_mcd_fgetc (mcd);
}

vpiHandle
vpi_register_cb (p_cb_data data)
{
  return VPI_THUNK->vpi_register_cb (data);
}

int
vpi_remove_cb (vpiHandle ref)
{
  return VPI_THUNK->vpi_remove_cb (ref);
}

void
vpi_sim_vcontrol (int operation, va_list ap)
{
  return VPI_THUNK->vpi_sim_vcontrol (operation, ap);
}

vpiHandle
vpi_handle (int type, vpiHandle ref)
{
  return VPI_THUNK->vpi_handle (type, ref);
}

vpiHandle
vpi_iterate (int type, vpiHandle ref)
{
  return VPI_THUNK->vpi_iterate (type, ref);
}

vpiHandle
vpi_scan (vpiHandle iter)
{
  return VPI_THUNK->vpi_scan (iter);
}

vpiHandle
vpi_handle_by_index (vpiHandle ref, int index)
{
  return VPI_THUNK->vpi_handle_by_index (ref, index);
}

void
vpi_get_time (vpiHandle obj, s_vpi_time *t)
{
  return VPI_THUNK->vpi_get_time (obj, t);
}

int
vpi_get (int property, vpiHandle ref)
{
  return VPI_THUNK->vpi_get (property, ref);
}

char *
vpi_get_str (int property, vpiHandle ref)
{
  return VPI_THUNK->vpi_get_str (property, ref);
}

void
vpi_get_value (vpiHandle expr, p_vpi_value value)
{
  return VPI_THUNK->vpi_get_value (expr, value);
}

vpiHandle
vpi_put_value (vpiHandle obj, p_vpi_value value, p_vpi_time when, int flags)
{
  return VPI_THUNK->vpi_put_value (obj, value, when, flags);
}

int
vpi_free_object (vpiHandle ref)
{
  return VPI_THUNK->vpi_free_object (ref);
}

int
vpi_get_vlog_info (p_vpi_vlog_info vlog_info_p)
{
  return VPI_THUNK->vpi_get_vlog_info (vlog_info_p);
}

int
vpi_chk_error (p_vpi_error_info info)
{
  return VPI_THUNK->vpi_chk_error (info);
}

vpiHandle
vpi_handle_by_name (char *name, vpiHandle scope)
{
  return VPI_THUNK->vpi_handle_by_name (name, scope);
}


int
vpi_printf (char *fmt, ...)
{
  va_list params;
  PLI_INT32 res;

  va_start (params, fmt);
  res = VPI_THUNK->vpi_vprintf (fmt, params);
  va_end (params);

  return res;
}

int
vpi_control (int op, ...)
{
  va_list params;
  int status;
  PLI_INT32 res;

  va_start (params, op);
  status = va_arg (params, int);
  res = VPI_THUNK->vpi_control_np (op, status);
  va_end (params);

  return res;
}
