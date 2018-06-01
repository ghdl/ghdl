/*  GRT C bindings.
    Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold.

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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

FILE *
__ghdl_get_stdout (void)
{
  return stdout;
}

FILE *
__ghdl_get_stdin (void)
{
  return stdin;
}

FILE *
__ghdl_get_stderr (void)
{
  return stderr;
}

int
__ghdl_snprintf_g (char *buf, unsigned int len, double val)
{
  snprintf (buf, len, "%g", val);
  return strlen (buf);
}

void
__ghdl_snprintf_fmtf (char *buf, unsigned int len,
		      const char *format, double v)
{
  snprintf (buf, len, format, v);
}

void
__ghdl_fprintf_g (FILE *stream, double val)
{
  fprintf (stream, "%g", val);
}

void
__ghdl_fprintf_clock (FILE *stream, int a, int b)
{
  fprintf (stream, "%3d.%03d", a, b);
}

#ifndef getc_unlocked
int
getc_unlocked (FILE *stream)
{
  return getc (stream);
}
#endif

#ifndef putc_unlocked
int
putc_unlocked (int c, FILE *stream)
{
  return putc (c, stream);
}
#endif

#ifndef feof_unlocked
int
feof_unlocked (FILE *stream)
{
  return feof (stream);
}
#endif

#ifndef WITH_GNAT_RUN_TIME
void
__gnat_last_chance_handler (void)
{
  abort ();
}

void *
__gnat_malloc (size_t size)
{
  void *res;
  res = malloc (size);
  return res;
}

void
__gnat_free (void *ptr)
{
  free (ptr);
}

void *
__gnat_realloc (void *ptr, size_t size)
{
  return realloc (ptr, size);
}

/* Unused imported symbols with gcc 8.1. */
int __gnat_binder_ss_count = 0;
size_t __gnat_default_ss_size = 0;
void *__gnat_default_ss_pool = NULL;

void
system__secondary_stack__ss_stackIP (void *t, size_t size)
{
}
#endif
