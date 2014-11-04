/*  GRT VPI C helpers.
    Copyright (C) 2003, 2004, 2005 Tristan Gingold & Felix Bertram

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
// Description: VPI interface for GRT runtime, "C" helpers
//              the main purpose of this code is to interface with the
//              Icarus Verilog Interactive (IVI) simulator GUI
//-----------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>

//-----------------------------------------------------------------------------
// VPI callback functions
typedef void *vpiHandle, *p_vpi_time, *p_vpi_value;
typedef struct t_cb_data {
      int reason;
      int (*cb_rtn)(struct t_cb_data*cb);
      vpiHandle obj;
      p_vpi_time time;
      p_vpi_value value;
      int index;
      char*user_data;
} s_cb_data, *p_cb_data;

//-----------------------------------------------------------------------------
// vpi thunking a la Icarus Verilog
#include <stdarg.h>
typedef void *s_vpi_time, *p_vpi_vlog_info, *p_vpi_error_info;
#define VPI_THUNK_MAGIC  (0x87836BA5)
struct t_vpi_systf_data;
void         vpi_register_systf  (const struct t_vpi_systf_data*ss);
void         vpi_vprintf         (const char*fmt, va_list ap);
unsigned int vpi_mcd_close       (unsigned int mcd);
char *       vpi_mcd_name        (unsigned int mcd);
unsigned int vpi_mcd_open        (char *name);
unsigned int vpi_mcd_open_x      (char *name, char *mode);
int          vpi_mcd_vprintf     (unsigned int mcd, const char*fmt, va_list ap);
int          vpi_mcd_fputc       (unsigned int mcd, unsigned char x);
int          vpi_mcd_fgetc       (unsigned int mcd);
vpiHandle    vpi_register_cb     (p_cb_data data);
int          vpi_remove_cb       (vpiHandle ref);
void         vpi_sim_vcontrol    (int operation, va_list ap);
vpiHandle    vpi_handle          (int type, vpiHandle ref);
vpiHandle    vpi_iterate         (int type, vpiHandle ref);
vpiHandle    vpi_scan            (vpiHandle iter);
vpiHandle    vpi_handle_by_index (vpiHandle ref, int index);
void         vpi_get_time        (vpiHandle obj, s_vpi_time*t);
int          vpi_get             (int property, vpiHandle ref);
char*        vpi_get_str         (int property, vpiHandle ref);
void         vpi_get_value       (vpiHandle expr, p_vpi_value value);
vpiHandle    vpi_put_value       (vpiHandle obj, p_vpi_value value,
                                  p_vpi_time when, int flags);
int          vpi_free_object     (vpiHandle ref);
int          vpi_get_vlog_info   (p_vpi_vlog_info vlog_info_p);
int          vpi_chk_error       (p_vpi_error_info info);
vpiHandle    vpi_handle_by_name  (char *name, vpiHandle scope);

typedef struct {
	int magic;
	void         (*vpi_register_systf) (const struct t_vpi_systf_data*ss);
	void         (*vpi_vprintf)        (const char*fmt, va_list ap);
	unsigned int (*vpi_mcd_close)      (unsigned int mcd);
	char*        (*vpi_mcd_name)       (unsigned int mcd);
	unsigned int (*vpi_mcd_open)       (char *name);
	unsigned int (*vpi_mcd_open_x)     (char *name, char *mode);
	int          (*vpi_mcd_vprintf)    (unsigned int mcd, const char*fmt, va_list ap);
	int          (*vpi_mcd_fputc)      (unsigned int mcd, unsigned char x);
	int          (*vpi_mcd_fgetc)      (unsigned int mcd);
	vpiHandle    (*vpi_register_cb)    (p_cb_data data);
	int          (*vpi_remove_cb)      (vpiHandle ref);
	void         (*vpi_sim_vcontrol)   (int operation, va_list ap);
	vpiHandle    (*vpi_handle)         (int type, vpiHandle ref);
	vpiHandle    (*vpi_iterate)        (int type, vpiHandle ref);
	vpiHandle    (*vpi_scan)           (vpiHandle iter);
	vpiHandle    (*vpi_handle_by_index)(vpiHandle ref, int index);
	void         (*vpi_get_time)       (vpiHandle obj, s_vpi_time*t);
	int          (*vpi_get)            (int property, vpiHandle ref);
	char*        (*vpi_get_str)        (int property, vpiHandle ref);
	void         (*vpi_get_value)      (vpiHandle expr, p_vpi_value value);
	vpiHandle    (*vpi_put_value)      (vpiHandle obj, p_vpi_value value,
	                                    p_vpi_time when, int flags);
	int          (*vpi_free_object)    (vpiHandle ref);
	int          (*vpi_get_vlog_info)  (p_vpi_vlog_info vlog_info_p);
	int          (*vpi_chk_error)      (p_vpi_error_info info);
	vpiHandle    (*vpi_handle_by_name) (char *name, vpiHandle scope);
} vpi_thunk, *p_vpi_thunk;

int vpi_register_sim(p_vpi_thunk tp);

static vpi_thunk thunkTable = 
{	VPI_THUNK_MAGIC,
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
	0, //vpi_chk_error,
	0 //vpi_handle_by_name
};

//-----------------------------------------------------------------------------
// VPI module load & startup
static void * module_open (const char *path);
static void * module_symbol (void *handle, const char *symbol);
static const char *module_error (void);

#if defined(__WIN32__)
#include <windows.h>
static void *
module_open (const char *path)
{
  return (void *)LoadLibrary (path);
}

static void *
module_symbol (void *handle, const char *symbol)
{
  return (void *)GetProcAddress ((HMODULE)handle, symbol);
}

static const char *
module_error (void)
{
  static char msg[256];

  FormatMessage
    (FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
     NULL,
     GetLastError (),
     MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
     (LPTSTR) &msg,
     sizeof (msg) - 1,
     NULL);
  return msg;
}
#else
#include <dlfcn.h>
static void *
module_open (const char *path)
{
  return dlopen (path, RTLD_LAZY);
}

static void *
module_symbol (void *handle, const char *symbol)
{
  return dlsym (handle, symbol);
}

static const char *
module_error (void)
{
  return dlerror ();
}
#endif

int
loadVpiModule (const char* modulename)
{
  static const char * const vpitablenames[] =
    {
      "_vlog_startup_routines", // with leading underscore: MacOSX
      "vlog_startup_routines"   // w/o  leading underscore: Linux
    };
  static const char * const vpithunknames[] =
    {
      "_vpi_register_sim",      // with leading underscore: MacOSX
      "vpi_register_sim"        // w/o  leading underscore: Linux
    };

  int i;	
  void* vpimod;

  fprintf (stderr, "loading VPI module '%s'\n", modulename);

  vpimod = module_open (modulename);

  if (vpimod == NULL)
    {
      const char *msg;

      msg = module_error ();

      fprintf (stderr, "%s\n", msg == NULL ? "unknown dlopen error" : msg);
      return -1;
    }

  for (i = 0; i < 2; i++) // try with and w/o leading underscores
    {
      void* vpithunk;
      void* vpitable;
	  
      vpitable = module_symbol (vpimod, vpitablenames[i]);
      vpithunk = module_symbol (vpimod, vpithunknames[i]);
	  
      if (vpithunk)
	{
	  typedef int (*funT)(p_vpi_thunk tp);
	  funT regsim;
	  
	  regsim = (funT)vpithunk;
	  regsim (&thunkTable);
	}
      else
	{
	  // this is not an error, as the register-mechanism
	  // is not standardized
	}
      
      if (vpitable)
	{
	  unsigned int tmp;
	  //extern void (*vlog_startup_routines[])();
	  typedef void (*vlog_startup_routines_t)(void);
	  vlog_startup_routines_t *vpifuns;
				
	  vpifuns = (vlog_startup_routines_t*)vpitable;
	  for (tmp = 0; vpifuns[tmp]; tmp++)
	    {
	      vpifuns[tmp]();
	    }
	  
	  fprintf (stderr, "VPI module loaded!\n");
	  return 0; // successfully registered VPI module
	}
    }
  fprintf (stderr, "vlog_startup_routines not found\n");
  return -1; // failed to register VPI module
}

void
vpi_printf (const char *fmt, ...)
{
  va_list params;

  va_start (params, fmt);
  vprintf (fmt, params);
  va_end (params);
}

//-----------------------------------------------------------------------------
// end of file

