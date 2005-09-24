/*  GRT C bindings for time.
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
#include <sys/times.h>
#include <unistd.h>

int
grt_get_clk_tck (void)
{
  return sysconf (_SC_CLK_TCK);
}

void
grt_get_times (int *wall, int *user, int *sys)
{
  clock_t res;
  struct tms buf;

  res = times (&buf);
  if (res == (clock_t)-1)
    {
      *wall = 0;
      *user = 0;
      *sys = 0;
    }
  else
    {
      *wall = res;
      *user = buf.tms_utime;
      *sys = buf.tms_stime;
    }
}

