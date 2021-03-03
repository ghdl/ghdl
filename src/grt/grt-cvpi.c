/*  GRT VPI C helpers.
    Copyright (C) 2003, 2004, 2005 Tristan Gingold & Felix Bertram

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
//-----------------------------------------------------------------------------
// Description: VPI interface for GRT runtime, "C" helpers
//              the main purpose of this code is to interface with the
//              Icarus Verilog Interactive (IVI) simulator GUI
//-----------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>

/* Define PLI_PROTOTYPES so that vpi_user.h do not import functions.  */
#define PLI_PROTOTYPES
#define PROTO_PARAMS(params) params
#define XXTERN extern

#include "vpi_user.h"
#include "vpi_thunk.h"
#include "grt-cdynload.h"

extern PLI_INT32 vpi_control_np (int op, int status);

static vpi_thunk __ghdl_vpi_thunk_v1 =
{
  vpi_register_systf,
  vpi_vprintf,
  vpi_mcd_close,
  vpi_mcd_name,
  vpi_mcd_open,
  0, //vpi_mcd_open_x,
  0, //vpi_mcd_vprintf,
  0, //vpi_mcd_fputc,
  0, //vpi_mcd_fgetc,
  vpi_register_cb,
  vpi_remove_cb,
  0, //vpi_sim_vcontrol,
  vpi_handle,
  vpi_iterate,
  vpi_scan,
  vpi_handle_by_index,
  vpi_get_time,
  vpi_get,
  vpi_get_str,
  vpi_get_value,
  vpi_put_value,
  vpi_free_object,
  vpi_get_vlog_info,
  vpi_chk_error,
  vpi_handle_by_name,
  vpi_control_np
};

//-----------------------------------------------------------------------------
// VPI module load & startup

#if defined (__APPLE__)
/* On Darwin: look in rpath.  */
#define LIBNAME "@rpath/libghdlvpi" DSO_EXT
#else
#define LIBNAME "libghdlvpi" DSO_EXT
#endif

static const char libghdlvpi_name[] = LIBNAME;

int
loadVpiModule (const char* modulename)
{
  static void *libghdlvpi_mod;
  int i;
  void *vpimod;

  fprintf (stderr, "loading VPI module '%s'\n", modulename);

  /* TODO: on windows, use SetDllDirectory with:
     - install dir (libdir) => add -DLIBDIR=xxx
     - exec path\lib => see windows_default_path
  */

  vpimod = grt_dynload_open (modulename);

  if (vpimod == NULL)
    {
      const char *msg = grt_dynload_error ();

      fprintf (stderr, "%s\n", msg == NULL ? "unknown dlopen error" : msg);
      return -1;
    }

  /* Try to load the libghdlvpi library and set the thunk.
     No need to load the library several times.  */
  if (libghdlvpi_mod == NULL)
    {
      libghdlvpi_mod = grt_dynload_open (libghdlvpi_name);
      if (libghdlvpi_mod != NULL)
	{
	  vpi_thunk **vpi_thunk_ptr;

	  for (i = 0; i < 2; i++)
	    {
	      vpi_thunk_ptr =
		grt_dynload_symbol (libghdlvpi_mod, &"_VPI_THUNK"[i]);

	      if (vpi_thunk_ptr != NULL)
		{
		  *vpi_thunk_ptr = &__ghdl_vpi_thunk_v1;
		  break;
		}
	    }
	  if (vpi_thunk_ptr == NULL)
	    fprintf (stderr, "warning: VPI_THUNK not found in %s\n",
		     libghdlvpi_name);
	}
    }

  for (i = 0; i < 2; i++) // try with and w/o leading underscores
    {
      void *vpitable =
	grt_dynload_symbol (vpimod, &"_vlog_startup_routines"[i]);

      if (vpitable)
	{
	  unsigned int j;
	  typedef void (*vlog_startup_routines_t)(void);
	  vlog_startup_routines_t *vpifuns;

	  vpifuns = (vlog_startup_routines_t*)vpitable;
	  for (j = 0; vpifuns[j]; j++)
	    vpifuns[j]();

	  fprintf (stderr, "VPI module loaded!\n");
	  return 0; // successfully registered VPI module
	}
    }
  fprintf (stderr, "vlog_startup_routines not found\n");
  return -1; // failed to register VPI module
}

PLI_INT32
vpi_vprintf (char *fmt, va_list ap)
{
  return vprintf (fmt, ap);
}

PLI_INT32
vpi_printf (char *fmt, ...)
{
  va_list params;
  PLI_INT32 res;

  va_start (params, fmt);
  res = vprintf (fmt, params);
  va_end (params);

  return res;
}

PLI_INT32
vpi_control (int op, ...)
{
  va_list params;
  int status;
  PLI_INT32 res;

  va_start (params, op);
  status = va_arg (params, int);
  res = vpi_control_np (op, status);
  va_end (params);

  return res;
}
