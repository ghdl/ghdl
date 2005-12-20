/*  GRT stack implementation for Win32 using fibers.
    Copyright (C) 2005 Tristan Gingold.

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

#include <windows.h>
#include <stdio.h>

struct stack_type
{
  LPVOID fiber; //  Win fiber.
  void (*func)(void *);  // Function
  void *arg; //  Function argument.
};

static struct stack_type  main_stack_context;
extern void grt_set_main_stack (struct stack_type *stack);

void grt_stack_init(void)
{
  main_stack_context.fiber = ConvertThreadToFiber (NULL);
  if (main_stack_context.fiber == NULL)
    {
      fprintf (stderr, "convertThreadToFiber failed (err=%lu)\n",
	       GetLastError ());
      abort ();
    }
  grt_set_main_stack (&main_stack_context);
}

static VOID __stdcall
grt_stack_loop (void *v_stack)
{
  struct stack_type *stack = (struct stack_type *)v_stack;
  while (1)
    {
      (*stack->func)(stack->arg);
    }
}

struct stack_type *
grt_stack_create (void (*func)(void *), void *arg) 
{
  struct stack_type *res;

  res = malloc (sizeof (struct stack_type));
  if (res == NULL)
    return NULL;
  res->func = func;
  res->arg = arg;
  res->fiber = CreateFiber (0, &grt_stack_loop, res);
  if (res->fiber == NULL)
    {
      free (res);
      return NULL;
    }
  return res;
}

void
grt_stack_switch (struct stack_type *to, struct stack_type *from)
{
  SwitchToFiber (to->fiber);
}

void
grt_stack_delete (struct stack_type *stack)
{
  DeleteFiber (stack->fiber);
  stack->fiber = NULL;
}

#ifndef WITH_GNAT_RUN_TIME
void __gnat_raise_storage_error(void)
{
   abort ();
}

void __gnat_raise_program_error(void)
{
   abort ();
}
#endif

