/*  GRT stacks implementation for linux and other *nix.
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
#define _GNU_SOURCE
#include <unistd.h>
#include <sys/mman.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/ucontext.h>
#include <stdlib.h>
//#include <stdint.h>

#ifdef __APPLE__
#define MAP_ANONYMOUS MAP_ANON
#endif

/* On x86, the stack growns downward.  */
#define STACK_GROWNS_DOWNWARD 1

#ifdef linux
/* If set, SIGSEGV is caught in order to automatically grow the stacks.  */
#define EXTEND_STACK 1
#endif

/* Defined in Grt.Stacks.  */
extern unsigned int stack_size;
extern unsigned int stack_max_size;

/* Size of a memory page.  */
static size_t page_size;

extern void grt_stack_error_grow_failed (void);
extern void grt_stack_error_null_access (void);
extern void grt_stack_error_memory_access (void);
extern void grt_overflow_error (void);

/* Definitions:
   The base of the stack is the address before the first available byte on the
     stack.  If the stack grows downward, the base is equal to the high bound.
*/
   
/* Per stack context.
   This context is allocated at the top (or bottom if the stack grows
   upward) of the stack.
   Therefore, the base of the stack can be easily deduced from the context.  */
struct stack_context
{
  /* The current stack pointer.  */
  void *cur_sp;
  /* The current stack length.  */
  size_t cur_length;
};

/* If MAP_ANONYMOUS is not defined, use /dev/zero. */
#ifndef MAP_ANONYMOUS
#define USE_DEV_ZERO
static int dev_zero_fd;
#define MAP_ANONYMOUS 0
#define MMAP_FILEDES dev_zero_fd
#else
#define MMAP_FILEDES -1
#endif

#if EXTEND_STACK
/* This is the current process being run.  */
extern struct stack_context *grt_get_current_process (void);

/* Stack used for signals.
   The stack must be different from the running stack, because we want to be
   able to extend the running stack.  When the stack need to be extended, the
   current stack pointer does not point to a valid address.  Therefore, the
   stack cannot be used or else a second SIGSEGV is generated while the
   arguments are pushed.  */
static unsigned long sig_stack[SIGSTKSZ / sizeof (long)];

/* Signal stack descriptor.  */
static stack_t sig_stk;

static struct sigaction prev_sigsegv_act;
static struct sigaction sigsegv_act;

/* The following code assumes stack grows downward.  */
#if !STACK_GROWNS_DOWNWARD
#error "Not implemented"
#endif

/* Handler for SIGSEGV signal, which grow the stack.  */
static void grt_sigsegv_handler (int signo, siginfo_t *info, void *ptr)
{
  static int in_handler;
  void *addr;
  struct stack_context *ctxt;
  void *stack_high;
  void *stack_low;
  void *n_low;
  size_t n_len;
  ucontext_t *uctxt = (ucontext_t *)ptr;

  in_handler++;

#ifdef __i386__
  /* Linux generates a SIGSEGV (!) for an overflow exception.  */
  if (uctxt->uc_mcontext.gregs[REG_TRAPNO] == 4)
    {
      grt_overflow_error ();
    }
#endif

  if (info == NULL || grt_get_current_process () == NULL || in_handler > 1)
    {
      /* We loose.  */
      sigaction (SIGSEGV, &prev_sigsegv_act, NULL);
      return;
    }

  addr = info->si_addr;
  
  /* Check ADDR belong to the stack.  */
  ctxt = grt_get_current_process ()->cur_sp;
  stack_high = (void *)(ctxt + 1);
  stack_low = stack_high - stack_max_size;
  if (addr > stack_high || addr < stack_low)
    {
      /* Out of the stack.  */
      if (addr < (void *)page_size)
	grt_stack_error_null_access ();
      else
	grt_stack_error_memory_access ();
    }
  /* Compute the address of the faulting page.  */
  n_low = (void *)((unsigned long)addr & ~(page_size - 1));

  /* Should not happen.  */
  if (n_low < stack_low)
    abort ();

  /*  Allocate one more page, if possible.  */
  if (n_low != stack_low)
    n_low -= page_size;

  /* Compute the new length.  */
  n_len = stack_high - n_low;

  if (mmap (n_low, n_len - ctxt->cur_length,  PROT_READ | PROT_WRITE,
	    MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, MMAP_FILEDES, 0)
      != n_low)
    {
      /* Cannot grow the stack.  */
      grt_stack_error_grow_failed ();
    }

  ctxt->cur_length = n_len;

  sigaction (SIGSEGV, &sigsegv_act, NULL);

  in_handler--;

  /* Hopes we can resume!  */
  return;
}

static void grt_signal_setup (void)
{
  sigsegv_act.sa_sigaction = &grt_sigsegv_handler;
  sigemptyset (&sigsegv_act.sa_mask);
  sigsegv_act.sa_flags = SA_ONESHOT | SA_ONSTACK | SA_SIGINFO;

  /* Use an alternate stack during signals.  */
  sig_stk.ss_sp = sig_stack;
  sig_stk.ss_size = sizeof (sig_stack);
  sig_stk.ss_flags = 0;
  sigaltstack (&sig_stk, NULL);

  /* We don't care about the return status.
     If the handler is not installed, then some feature are lost.  */
  sigaction (SIGSEGV, &sigsegv_act, &prev_sigsegv_act);
}
#endif

/* Context for the main stack.  */
#ifdef USE_THREADS
#define THREAD __thread
#else
#define THREAD
#endif
static THREAD struct stack_context main_stack_context;

extern void grt_set_main_stack (struct stack_context *stack);

void
grt_stack_new_thread (void)
{
  main_stack_context.cur_sp = NULL;
  main_stack_context.cur_length = 0;
  grt_set_main_stack (&main_stack_context);
}

void
grt_stack_init (void)
{
  size_t pg_round;

  page_size = getpagesize ();
  pg_round = page_size - 1;

  /* Align size.  */
  stack_size = (stack_size + pg_round) & ~pg_round;
  stack_max_size = (stack_max_size + pg_round) & ~pg_round;

  /* Set mimum values.  */
  if (stack_size < 2 * page_size)
    stack_size = 2 * page_size;
  if (stack_max_size < (stack_size + 2 * page_size))
    stack_max_size = stack_size + 2 * page_size;

  /* Initialize the main stack context.  */
  main_stack_context.cur_sp = NULL;
  main_stack_context.cur_length = 0;
  grt_set_main_stack (&main_stack_context);

#ifdef USE_DEV_ZERO
  dev_zero_fd = open ("/dev/zero", O_RDWR);
  if (dev_zero_fd < 0)
    abort ();
#endif

#if EXTEND_STACK
  grt_signal_setup ();
#endif
}

/* Allocate a stack.
   Called by i386.S  */
struct stack_context *
grt_stack_allocate (void)
{
  struct stack_context *res;
  void *r;
  void *base;

  /* Allocate the stack, but without any rights.  This is a guard.  */
  base = (void *)mmap (NULL, stack_max_size, PROT_NONE,
		       MAP_PRIVATE | MAP_ANONYMOUS, MMAP_FILEDES, 0);

  if (base == (void *)-1)
    return NULL;

  /* Set rights on the allocated stack.  */
#if STACK_GROWNS_DOWNWARD
  r = base + stack_max_size - stack_size;
#else
  r = base;
#endif
  if (mmap (r, stack_size,  PROT_READ | PROT_WRITE,
	    MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, MMAP_FILEDES, 0)
      != r)
    return NULL;

#if STACK_GROWNS_DOWNWARD
  res = (struct stack_context *)
    (base + stack_max_size - sizeof (struct stack_context));
#else
  res = (struct stack_context *)(base + sizeof (struct stack_context));
#endif

#ifdef __ia64__
  /* Also allocate BSP.  */
  if (mmap (base, page_size, PROT_READ | PROT_WRITE,
	    MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, MMAP_FILEDES, 0) != base)
    return NULL;
#endif

  res->cur_sp = (void *)res;
  res->cur_length = stack_size;
  return res;
}

#include <setjmp.h>
static int run_env_en;
static jmp_buf run_env;

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

  run_env_en = 1;
  res = setjmp (run_env);
  if (res == 0)
    res = (*func)();
  run_env_en = 0;
  return res;
}

