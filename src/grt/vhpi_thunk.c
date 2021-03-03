/* GHDL Run Time (GRT) - VHPI thunks
   Copyright (C) 2021 Marlon James

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

/* Define PLI_PROTOTYPES so that vhpi_user.h does not import functions.  */
#define PLI_PROTOTYPES
#define XXTERN extern

#include "vhpi_user.h"
#include "vhpi_thunk.h"

// the function pointer table
vhpi_thunk *VHPI_THUNK;

// VHPI thunks

int
vhpi_assert (vhpiSeverityT severity, char *formatmsg, ...)
{
  va_list args;
  int res;

  va_start (args, formatmsg);
  res = VHPI_THUNK->vhpi_vassert (severity, formatmsg, args);
  va_end(args);

  return res;
}

vhpiHandleT vhpi_register_cb (vhpiCbDataT *cb_data_p, int32_t flags)
{
  return VHPI_THUNK->vhpi_register_cb (cb_data_p, flags);
}

int
vhpi_remove_cb (vhpiHandleT cb_obj)
{
  return VHPI_THUNK->vhpi_remove_cb (cb_obj);
}

int
vhpi_disable_cb (vhpiHandleT cb_obj)
{
  return VHPI_THUNK->vhpi_disable_cb (cb_obj);
}

int
vhpi_enable_cb (vhpiHandleT cb_obj)
{
  return VHPI_THUNK->vhpi_enable_cb (cb_obj);
}

int
vhpi_get_cb_info (vhpiHandleT object, vhpiCbDataT *cb_data_p)
{
  return VHPI_THUNK->vhpi_get_cb_info (object, cb_data_p);
}

int
vhpi_sens_first (vhpiValueT *sens)
{
  return VHPI_THUNK->vhpi_sens_first (sens);
}

int
vhpi_sens_zero (vhpiValueT *sens)
{
  return VHPI_THUNK->vhpi_sens_zero (sens);
}

int
vhpi_sens_clr (int obj, vhpiValueT *sens)
{
  return VHPI_THUNK->vhpi_sens_clr (obj, sens);
}

int
vhpi_sens_set (int obj, vhpiValueT *sens)
{
  return VHPI_THUNK->vhpi_sens_set (obj, sens);
}

int
vhpi_sens_isset (int obj, vhpiValueT *sens)
{
  return VHPI_THUNK->vhpi_sens_isset (obj, sens);
}

vhpiHandleT
vhpi_handle_by_name (const char *name, vhpiHandleT scope)
{
  return VHPI_THUNK->vhpi_handle_by_name (name, scope);
}

vhpiHandleT
vhpi_handle_by_index (vhpiOneToManyT itRel, vhpiHandleT parent, int32_t indx)
{
  return VHPI_THUNK->vhpi_handle_by_index (itRel, parent, indx);
}

vhpiHandleT
vhpi_handle (vhpiOneToOneT type, vhpiHandleT referenceHandle)
{
  return VHPI_THUNK->vhpi_handle (type, referenceHandle);
}

vhpiHandleT
vhpi_iterator (vhpiOneToManyT type, vhpiHandleT referenceHandle)
{
  return VHPI_THUNK->vhpi_iterator (type, referenceHandle);
}

vhpiHandleT
vhpi_scan (vhpiHandleT iterator)
{
  return VHPI_THUNK->vhpi_scan (iterator);
}

vhpiIntT
vhpi_get (vhpiIntPropertyT property, vhpiHandleT object)
{
  return VHPI_THUNK->vhpi_get (property, object);
}

const vhpiCharT *
vhpi_get_str (vhpiStrPropertyT property, vhpiHandleT object)
{
  return VHPI_THUNK->vhpi_get_str (property, object);
}

vhpiRealT
vhpi_get_real (vhpiRealPropertyT property, vhpiHandleT object)
{
  return VHPI_THUNK->vhpi_get_real (property, object);
}

vhpiPhysT
vhpi_get_phys (vhpiPhysPropertyT property, vhpiHandleT object)
{
  return VHPI_THUNK->vhpi_get_phys (property, object);
}

int
vhpi_protected_call (vhpiHandleT varHdl, vhpiUserFctT userFct, void *userData)
{
  return VHPI_THUNK->vhpi_protected_call (varHdl, userFct, userData);
}

int
vhpi_get_value (vhpiHandleT expr, vhpiValueT *value_p)
{
  return VHPI_THUNK->vhpi_get_value (expr, value_p);
}

int
vhpi_put_value (vhpiHandleT object, vhpiValueT *value_p, vhpiPutValueModeT mode)
{
  return VHPI_THUNK->vhpi_put_value (object, value_p, mode);
}

int
vhpi_schedule_transaction (vhpiHandleT drivHdl, vhpiValueT *value_p,
                           uint32_t numValues, vhpiTimeT *delayp,
                           vhpiDelayModeT delayMode, vhpiTimeT *pulseRejp)
{
  return VHPI_THUNK->vhpi_schedule_transaction (drivHdl, value_p, numValues,
                                                delayp, delayMode, pulseRejp);
}

int
vhpi_format_value (const vhpiValueT *in_value_p, vhpiValueT *out_value_p)
{
  return VHPI_THUNK->vhpi_format_value (in_value_p, out_value_p);
}

void
vhpi_get_time (vhpiTimeT *time_p, long *cycles)
{
  return VHPI_THUNK->vhpi_get_time (time_p, cycles);
}

int
vhpi_get_next_time (vhpiTimeT *time_p)
{
  return VHPI_THUNK->vhpi_get_next_time (time_p);
}

int
vhpi_control (vhpiSimControlT command, ...)
{
  va_list args;
  int status;
  int res;

  va_start (args, command);
  status = va_arg (args, int);
  res = VHPI_THUNK->vhpi_control_internal (command, status);
  va_end (args);

  return res;
}

int
vhpi_printf (const char *format, ...)
{
  va_list args;
  int res;

  va_start (args, format);
  res = VHPI_THUNK->vhpi_vprintf (format, args);
  va_end (args);

  return res;
}

int
vhpi_vprintf (const char *format, va_list args)
{
  return VHPI_THUNK->vhpi_vprintf (format, args);
}
