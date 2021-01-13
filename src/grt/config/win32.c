/*  GRT stack implementation for Win32 using fibers.
    Copyright (C) 2005 - 2014 Tristan Gingold.

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

    As a special exception, if other files instantiate generics from this
    unit, or you link this unit with other files to produce an executable,
    this unit does not by itself cause the resulting executable to be
    covered by the GNU General Public License. This exception does not
    however invalidate any other reasons why the executable file might be
    covered by the GNU Public License.
*/

#include <windows.h>
#include <winbase.h>
#include <dbghelp.h>
#include <stdio.h>
#include <setjmp.h>
#include <assert.h>
#include <excpt.h>

#include "grt_itf.h"

static int run_env_en;
static jmp_buf run_env;

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

/* Save bactktrace from CTXT to BT, the first SKIP frames are skipped.
   We need to use StackWalk64 as apparently CaptureStackBackTrace doesn't
   work over JIT'ed code.  I suppose it checks whether PC belongs to the text
   section of an image.  */

static void
get_bt_from_context (struct backtrace_addrs *bt, CONTEXT *ctxt, int skip)
{
  STACKFRAME64 frame;
  unsigned mach;

  bt->size = 0;
  bt->skip = 0;
  memset (&frame, 0, sizeof (frame));

#ifdef __i386__
  mach = IMAGE_FILE_MACHINE_I386;

  frame.AddrPC.Offset = ctxt->Eip;
  frame.AddrPC.Mode = AddrModeFlat;
  frame.AddrFrame.Offset = ctxt->Ebp;
  frame.AddrFrame.Mode = AddrModeFlat;
  frame.AddrStack.Offset = ctxt->Esp;
  frame.AddrStack.Mode = AddrModeFlat;

#elif defined (__x86_64__)
  mach = IMAGE_FILE_MACHINE_AMD64;

  frame.AddrPC.Offset = ctxt->Rip;
  frame.AddrPC.Mode = AddrModeFlat;
  frame.AddrFrame.Offset = ctxt->Rsp;
  frame.AddrFrame.Mode = AddrModeFlat;
  frame.AddrStack.Offset = ctxt->Rsp;
  frame.AddrStack.Mode = AddrModeFlat;

#else
#  warning "platform not supported"
  return;
#endif

  while (bt->size < sizeof (bt->addrs) / sizeof (bt->addrs[0]))
    {
      if (skip > 0)
	skip--;
      else
	bt->addrs[bt->size++] = (void *) frame.AddrPC.Offset;

      if (!StackWalk64 (mach, GetCurrentProcess (), GetCurrentThread (),
			&frame, ctxt, NULL, NULL, NULL, NULL))
	break;
    }
}

static EXCEPTION_DISPOSITION
ghdl_SEH_handler (struct _EXCEPTION_RECORD* ExceptionRecord,
		  void *EstablisherFrame,
		  struct _CONTEXT* ContextRecord,
		  void *DispatcherContext)
{
  struct backtrace_addrs bt;
  const char *msg = "";

  switch (ExceptionRecord->ExceptionCode)
    {
    case EXCEPTION_ACCESS_VIOLATION:
      /* Pc is ExceptionRecord->ExceptionAddress.  */
      get_bt_from_context (&bt, ContextRecord, 1);
      grt_null_access_error (&bt);
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
      get_bt_from_context (&bt, ContextRecord, 1);
      grt_overflow_error (&bt);
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

void
__ghdl_maybe_return_via_longjump (int val)
{
  if (run_env_en)
    longjmp (run_env, val);
}

int
__ghdl_run_through_longjump (int (*func)(void))
{
  int res;

#ifdef __i386__
  /* Install an SEH handler.  */
  struct exception_registration er;
  struct exception_registration *prev;

  /* Get current handler.  */
  asm volatile ("mov %%fs:(0),%0" : "=r" (prev));

  /* Build regisration.  */
  er.prev = prev;
  er.handler = ghdl_SEH_handler;

  /* Register.  */
  asm volatile ("mov %0,%%fs:(0)" : : "r" (&er));
#endif

  run_env_en = 1;
  res = setjmp (run_env);
  if (res == 0)
    res = (*func)();
  run_env_en = 0;

#ifdef __i386__
  /* Restore.  */
  asm volatile ("mov %0,%%fs:(0)" : : "r" (prev));
#endif

  return res;
}

void
grt_save_backtrace (struct backtrace_addrs *bt, int skip)
{
  /* FIXME
  testsuite/gna/issue635 fails on GitHub Actions when executed with
  LLVM backend on MINGW64 (MSYS2). GHDL returns '3', instead of '0'.
  This dummy printf fixes it, surprisingly.
  See https://github.com/ghdl/ghdl/pull/1516
  */
  printf("");

  CONTEXT ctxt;

  RtlCaptureContext (&ctxt);
  get_bt_from_context (bt, &ctxt, skip + 1);
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
