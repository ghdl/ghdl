/*  GRT replacements for GNAT rts.
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
#include <stdlib.h>

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
