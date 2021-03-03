/*  GRT VHPI C helpers.
    Copyright (C) 2021 Marlon James

    GHDL is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Public License as published by the Free
    Software Foundation; either version 2, or (at your option) any later
    version.

    GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with GCC; see the file COPYING.  If not, write to the Free
    Software Foundation, 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.
*/
//-----------------------------------------------------------------------------
// Description: VHPI interface for GRT runtime, "C" helpers
//              for loading VHPI foreign applications
//              and libraries of foreign models,
//              and implementing variadic functions in vhpi_user.h
//-----------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>

// Define PLI_PROTOTYPES so that vhpi_user.h does not import functions.
#define PLI_PROTOTYPES
#define XXTERN extern

#include "vhpi_user.h"
#include "vhpi_thunk.h"
#include "grt-cdynload.h"

// See grt-vhpi.ads
extern int Vhpi_Control_Internal (int command, int status);
extern int Vhpi_Assert_Internal (int severity, const char *msg);

// Forward declaration
int vhpi_vassert (vhpiSeverityT severity, char *formatmsg,
                  va_list args);

static vhpi_thunk __ghdl_vhpi_thunk_v1 =
{
  vhpi_vassert,
  vhpi_register_cb,
  vhpi_remove_cb,
  vhpi_disable_cb,
  vhpi_enable_cb,
  vhpi_get_cb_info,
  0, //vhpi_sens_first,
  0, //vhpi_sens_zero,
  0, //vhpi_sens_clr,
  0, //vhpi_sens_set,
  0, //vhpi_sens_isset,
  vhpi_handle_by_name,
  vhpi_handle_by_index,
  vhpi_handle,
  vhpi_iterator,
  vhpi_scan,
  vhpi_get,
  vhpi_get_str,
  vhpi_get_real,
  vhpi_get_phys,
  vhpi_protected_call,
  vhpi_get_value,
  vhpi_put_value,
  vhpi_schedule_transaction,
  vhpi_format_value,
  vhpi_get_time,
  vhpi_get_next_time,
  Vhpi_Control_Internal,
  vhpi_vprintf,
  vhpi_is_printable,
  vhpi_compare_handles,
  vhpi_check_error,
  vhpi_release_handle,
  vhpi_create,
  vhpi_register_foreignf,
  vhpi_get_foreignf_info,
  vhpi_get_data,
  vhpi_put_data
};

//-----------------------------------------------------------------------------
// VHPI module load & startup

// VHPI thunks are combined into libghdlvpi
#if defined (__APPLE__)
// On Darwin: look in rpath.
#define LIBNAME "@rpath/libghdlvpi" DSO_EXT
#else
#define LIBNAME "libghdlvpi" DSO_EXT
#endif

static const char libghdlvpi_name[] = LIBNAME;

int
loadVhpiModule (const char* libname, const char* entrypoint)
{
  static void *libghdlvpi_mod;
  int i;
  void *vhpimod;

  fprintf (stderr, "loading VHPI library '%s'", libname);
  if (entrypoint)
    {
      fprintf (stderr, " with registration function '%s'", entrypoint);
    }
  fprintf (stderr, "\n");

  // TODO: on windows, use SetDllDirectory with:
  // - install dir (libdir) => add -DLIBDIR=xxx
  // - exec path\lib => see windows_default_path

  vhpimod = grt_dynload_open (libname);

  if (vhpimod == NULL)
    {
      const char *msg = grt_dynload_error ();

      fprintf (stderr, "%s\n", msg == NULL ? "unknown dlopen error" : msg);
      return -1;
    }

  // Try to load the libghdlvpi library and set the thunk pointer.
  // No need to load the library several times.
  if (libghdlvpi_mod == NULL)
    {
      libghdlvpi_mod = grt_dynload_open (libghdlvpi_name);
      if (libghdlvpi_mod != NULL)
        {
          vhpi_thunk **vhpi_thunk_ptr;

          for (i = 0; i < 2; i++)
            {
              vhpi_thunk_ptr = grt_dynload_symbol (libghdlvpi_mod, &"_VHPI_THUNK"[i]);

              if (vhpi_thunk_ptr != NULL)
                {
                  *vhpi_thunk_ptr = &__ghdl_vhpi_thunk_v1;
                  break;
                }
            }
          if (vhpi_thunk_ptr == NULL)
            fprintf (stderr, "warning: VHPI_THUNK not found in %s\n",
              libghdlvpi_name);
        }
    }

  if (entrypoint)
    {
      void *regfunc = grt_dynload_symbol (vhpimod, entrypoint);

      if (regfunc)
        {
          ((vhpiRegistrationFctT)regfunc)();
          fprintf (stderr, "VHPI module loaded and registration function called!\n");
          return 0; // successfully registered VHPI module
        }
      fprintf (stderr, "registration function '%s' not found\n", entrypoint);
      return -1; // failed to register VHPI module
    }

  for (i = 0; i < 2; i++) // try with and w/o leading underscores
    {
      void *vhpitable =
        grt_dynload_symbol (vhpimod, &"_vhpi_startup_routines"[i]);

      if (vhpitable)
        {
          unsigned int idx;
          vhpiRegistrationFctT *vhpifuncs;

          vhpifuncs = (vhpiRegistrationFctT*)vhpitable;
          for (idx = 0; vhpifuncs[idx]; idx++)
            {
              vhpifuncs[idx]();
            }

          fprintf (stderr, "VHPI module loaded and vhpi_startup_routines functions called!\n");
          return 0; // successfully registered VHPI module
        }
    }
  fprintf (stderr, "vhpi_startup_routines not found\n");
  return -1; // failed to register VHPI module
}

//-----------------------------------------------------------------------------
// VHPI functions

#define ASSERTMSG_SIZE    512
static char assertmsg_buff[ASSERTMSG_SIZE];
static const char* default_msg = "VHPI foreign application error";

int vhpi_vassert (vhpiSeverityT severity, char *formatmsg,
                  va_list args)
{
  if (formatmsg == NULL) {
    return Vhpi_Assert_Internal(severity, default_msg);
  }

  // construct assert message in the buffer
  vsnprintf(assertmsg_buff, ASSERTMSG_SIZE, formatmsg, args);
 
  return Vhpi_Assert_Internal(severity, assertmsg_buff);
}

int
vhpi_assert (vhpiSeverityT severity, char *formatmsg, ...)
{
  va_list args;
  int res;

  va_start (args, formatmsg);
  res = vhpi_vassert (severity, formatmsg, args);
  va_end(args);

  return res;
}

int
vhpi_control (vhpiSimControlT command, ...)
{
  va_list args;
  int status;
  int res;

  va_start (args, command);
  status = va_arg (args, int);
  res = Vhpi_Control_Internal (command, status);
  va_end (args);

  return res;
}

int
vhpi_printf (const char *format, ...)
{
  va_list args;
  int res;

  va_start (args, format);
  res = vprintf (format, args);
  va_end (args);

  return res;
}

int
vhpi_vprintf (const char *format, va_list args)
{
  return vprintf (format, args);
}
