/*  Memory segment handling.
    Copyright (C) 2006 Tristan Gingold.

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
#ifndef WINNT

#define _GNU_SOURCE
#include <sys/mman.h>
#include <stddef.h>
/* #include <stdio.h> */

/* TODO: init (get pagesize)
    round size,
   set rights.
*/

#if defined(__APPLE__) || defined(__OpenBSD__)
#define MAP_ANONYMOUS MAP_ANON
#else
#define HAVE_MREMAP
#endif

#ifndef HAVE_MREMAP
#include <string.h>
#endif

void *
mmap_malloc (int size)
{
  void *res;
  res = mmap (NULL, size, PROT_READ | PROT_WRITE,
	      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  /* printf ("mmap (%d) = %p\n", size, res); */
  if (res == MAP_FAILED)
    return NULL;
  return res;
}

void *
mmap_realloc (void *ptr, int old_size, int size)
{
  void *res;
#ifdef HAVE_MREMAP
  res = mremap (ptr, old_size, size, MREMAP_MAYMOVE);
#else
  res = mmap (NULL, size, PROT_READ | PROT_WRITE,
	      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (res == MAP_FAILED)
    return NULL;
  memcpy (res, ptr, old_size);
  munmap (ptr, old_size);
#endif
  /* printf ("mremap (%p, %d, %d) = %p\n", ptr, old_size, size, res); */
#if 0
  if (res == MAP_FAILED)
    return NULL;
#endif
  return res;
}

void
mmap_free (void * ptr, int size)
{
  munmap (ptr, size);
}

void
mmap_rx (void *ptr, int size)
{
  mprotect (ptr, size, PROT_READ | PROT_EXEC);
}

#else
#include <windows.h>

void *
mmap_malloc (int size)
{
  void *res;
  res = VirtualAlloc (NULL, size, 
		      MEM_COMMIT | MEM_RESERVE,
		      PAGE_READWRITE);
  return res;
}

void *
mmap_realloc (void *ptr, int old_size, int size)
{
  void *res;

  res = VirtualAlloc (NULL, size, 
		      MEM_COMMIT | MEM_RESERVE,
		      PAGE_READWRITE);

  if (ptr != NULL)
    {
      CopyMemory (res, ptr, size > old_size ? old_size : size);
      VirtualFree (ptr, old_size, MEM_RELEASE);
    }

  return res;
}

void
mmap_free (void * ptr, int size)
{
  VirtualFree (ptr, size, MEM_RELEASE);
}

void
mmap_rx (void *ptr, int size)
{
  DWORD old;

  /* This is not supported on every version.
     In case of failure, this should still work.  */
  VirtualProtect (ptr, size,  PAGE_EXECUTE_READ, &old);
}
#endif
