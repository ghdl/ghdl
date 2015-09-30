/*  Longjump/Setjump wrapper
    Copyright (C) 2002 - 2015 Tristan Gingold.

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

    As a special exception, if other files instantiate generics from this
    unit, or you link this unit with other files to produce an executable,
    this unit does not by itself cause the resulting executable to be
    covered by the GNU General Public License. This exception does not
    however invalidate any other reasons why the executable file might be
    covered by the GNU Public License.
*/

#define _GNU_SOURCE
#include <stddef.h>
#include <signal.h>
#include <fcntl.h>

#if defined (__linux__) && defined (__i386__)
/* On i386/Linux, the context must be inspected.  */
#include <sys/ucontext.h>
#endif

/* There is a simple setjmp/longjmp mechanism used to report failures.
   We have the choice between 3 mechanisms:
   * USE_BUITLIN_SJLJ: gcc builtin setjmp/longjmp, very fast but gcc specific.
   * USE__SETJMP: _setjmp/_longjmp
   * USE_SETJMP: setjmp/longjmp, slower because signals mask is saved/restored.
*/

#if defined (__GNUC__) && !defined (__clang__)
# define USE_BUILTIN_SJLJ
#else
# define USE__SETJMP
#endif

#ifdef USE_BUILTIN_SJLJ
typedef void *JMP_BUF[5];
static int sjlj_val;
# define SETJMP(BUF) (__builtin_setjmp (BUF), sjlj_val)
# define LONGJMP(BUF, VAL) \
  do { sjlj_val = (VAL); __builtin_longjmp (BUF, 1); } while (0)
#else
# include <setjmp.h>
typedef jmp_buf JMP_BUF;
# ifdef USE__SETJMP
#  define SETJMP _setjmp
#  define LONGJMP _longjmp
# elif defined (USE_SETJMP)
#  define SETJMP setjmp
#  define LONGJMP longjmp
# else
#  error "SETJMP/LONGJMP not configued"
# endif
#endif

static int run_env_en;
static JMP_BUF run_env;

extern void grt_overflow_error (void);
extern void grt_null_access_error (void);

#ifdef __APPLE__
#define NEED_SIGFPE_HANDLER
#endif

static struct sigaction prev_sigfpe_act;

/* Handler for SIGFPE signal.
   It is also raised in case of overflow (i386 linux).  */
static void grt_overflow_handler (int signo, siginfo_t *info, void *ptr)
{
  grt_overflow_error ();
}

static struct sigaction prev_sigsegv_act;

/* Posix handler for overflow.  This is used only by mcode.  */
static void grt_sigsegv_handler (int signo, siginfo_t *info, void *ptr)
{
#if defined (__linux__) && defined (__i386__)
  ucontext_t *uctxt = (ucontext_t *)ptr;

  /* Linux generates a SIGSEGV (!) for an overflow exception.  */
  if (uctxt->uc_mcontext.gregs[REG_TRAPNO] == 4)
    {
      grt_overflow_error ();
    }
#endif

  /* We loose.  */
  grt_null_access_error ();
}

static void grt_signal_setup (void)
{
  {
    struct sigaction sigsegv_act;

    sigsegv_act.sa_sigaction = &grt_sigsegv_handler;
    sigemptyset (&sigsegv_act.sa_mask);
    sigsegv_act.sa_flags = SA_ONSTACK | SA_SIGINFO;
#ifdef SA_ONESHOT
    sigsegv_act.sa_flags |= SA_ONESHOT;
#elif defined (SA_RESETHAND)
    sigsegv_act.sa_flags |= SA_RESETHAND;
#endif

    /* We don't care about the return status.
       If the handler is not installed, then some feature are lost.  */
    sigaction (SIGSEGV, &sigsegv_act, &prev_sigsegv_act);
  }

#ifdef NEED_SIGFPE_HANDLER
  {
    struct sigaction sig_ovf_act;

    sig_ovf_act.sa_sigaction = &grt_overflow_handler;
    sigemptyset (&sig_ovf_act.sa_mask);
    sig_ovf_act.sa_flags = SA_SIGINFO;

    sigaction (SIGFPE, &sig_ovf_act, &prev_sigfpe_act);
  }
#endif
}

static void grt_signal_restore (void)
{
  sigaction (SIGSEGV, &prev_sigsegv_act, NULL);

#ifdef NEED_SIGFPE_HANDLER
  sigaction (SIGFPE, &prev_sigfpe_act, NULL);
#endif
}

void
__ghdl_maybe_return_via_longjump (int val)
{
  if (run_env_en)
    LONGJMP (run_env, val);
}

int
__ghdl_run_through_longjump (int (*func)(void))
{
  int res;

  run_env_en = 1;
  grt_signal_setup ();
  res = SETJMP (run_env);
  if (res == 0)
    res = (*func)();
  grt_signal_restore ();
  run_env_en = 0;
  return res;
}
