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
#include <setjmp.h>
#include <assert.h>
#include <excpt.h>

static EXCEPTION_DISPOSITION
ghdl_SEH_handler (struct _EXCEPTION_RECORD* ExceptionRecord,
		  void *EstablisherFrame,
		  struct _CONTEXT* ContextRecord,
		  void *DispatcherContext);

struct exception_registration
{
  struct exception_registration *prev;
  void *handler;
};

struct stack_type
{
  LPVOID fiber; //  Win fiber.
  void (*func)(void *);  // Function
  void *arg; //  Function argument.
};

static struct stack_type main_stack_context;
static struct stack_type *current;
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
  current = &main_stack_context;
}

static VOID __stdcall
grt_stack_loop (void *v_stack)
{
  struct stack_type *stack = (struct stack_type *)v_stack;
  struct exception_registration er;
  struct exception_registration *prev;

  /* Get current handler.  */
  asm ("mov %%fs:(0),%0" : "=r" (prev));

  /* Build regisration.  */
  er.prev = prev;
  er.handler = ghdl_SEH_handler;

  /* Register.  */
  asm ("mov %0,%%fs:(0)" : : "r" (&er));

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

static int run_env_en;
static jmp_buf run_env;
static int need_longjmp;

void
grt_stack_switch (struct stack_type *to, struct stack_type *from)
{
  assert (current == from);
  current = to;
  SwitchToFiber (to->fiber);
  if (from == &main_stack_context && need_longjmp)
    {
      /* We returned to do the longjump.  */
      current = &main_stack_context;
      longjmp (run_env, need_longjmp);
    }
}

void
grt_stack_delete (struct stack_type *stack)
{
  DeleteFiber (stack->fiber);
  stack->fiber = NULL;
}

void
__ghdl_maybe_return_via_longjump (int val)
{
  if (!run_env_en)
    return;

  if (current != &main_stack_context)
    {
      /* We are allowed to jump only in the same stack.
	 First switch back to the main thread.  */
      need_longjmp = val;
      SwitchToFiber (main_stack_context.fiber);
    }
  else
    longjmp (run_env, val);
}

extern void grt_stack_error_grow_failed (void);
extern void grt_stack_error_null_access (void);
extern void grt_stack_error_memory_access (void);
extern void grt_overflow_error (void);

static EXCEPTION_DISPOSITION
ghdl_SEH_handler (struct _EXCEPTION_RECORD* ExceptionRecord,
		  void *EstablisherFrame,
		  struct _CONTEXT* ContextRecord,
		  void *DispatcherContext)
{
  const char *msg = "";

  switch (ExceptionRecord->ExceptionCode)
    {
    case EXCEPTION_ACCESS_VIOLATION:
      if (ExceptionRecord->ExceptionInformation[1] == 0)
	grt_stack_error_null_access ();
      else
	grt_stack_error_memory_access ();
      break;
          
    case EXCEPTION_FLT_DENORMAL_OPERAND:
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
    case EXCEPTION_FLT_INVALID_OPERATION:
    case EXCEPTION_FLT_OVERFLOW:
    case EXCEPTION_FLT_STACK_CHECK:
    case EXCEPTION_FLT_UNDERFLOW:
      msg = "floating point error";
      break;
     
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
      msg = "division by 0";
      break;
     
    case EXCEPTION_INT_OVERFLOW:
      grt_overflow_error ();
      break;
           
    case EXCEPTION_STACK_OVERFLOW:
      msg = "stack overflow";
      break;
     
    default:
      msg = "unknown reason";
      break;
    }

  /* FIXME: is it correct?  */
  fprintf (stderr, "exception raised: %s\n", msg);

  __ghdl_maybe_return_via_longjump (1);
  return 0; /* This is never reached, avoid compiler warning  */
}

int
__ghdl_run_through_longjump (int (*func)(void))
{
  int res;
  struct exception_registration er;
  struct exception_registration *prev;

  /* Get current handler.  */
  asm ("mov %%fs:(0),%0" : "=r" (prev));

  /* Build regisration.  */
  er.prev = prev;
  er.handler = ghdl_SEH_handler;

  /* Register.  */
  asm ("mov %0,%%fs:(0)" : : "r" (&er));

  run_env_en = 1;
  res = setjmp (run_env);
  if (res == 0)
    res = (*func)();
  run_env_en = 0;

  /* Restore.  */
  asm ("mov %0,%%fs:(0)" : : "r" (prev));

  return res;
}

#include <math.h>

double acosh (double x)
{
  return log (x + sqrt (x*x - 1));
}

double asinh (double x)
{
  return log (x + sqrt (x*x + 1));
}

double atanh (double x)
{
  return log ((1 + x) / (1 - x)) / 2;
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

