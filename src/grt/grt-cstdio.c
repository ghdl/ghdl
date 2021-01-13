/*  GRT C bindings for stdio
    Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold.

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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

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

#ifdef __cplusplus
}
#endif
