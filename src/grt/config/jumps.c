/*  Longjump/Setjump wrapper
    Copyright (C) 2002 - 2015 Tristan Gingold.

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

#define _GNU_SOURCE
#include <stddef.h>
#include <signal.h>
#include <fcntl.h>

#if ( (defined (__linux__) && defined (__GLIBC__) ) || defined (__APPLE__) ) && !defined (__ANDROID__)
#define HAVE_BACKTRACE 1
#include <sys/ucontext.h>
#endif

#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#endif

#include "grt_itf.h"

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
# define SETJMP(BUF) (sjlj_val = 0, __builtin_setjmp (BUF), sjlj_val)
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

#ifdef __APPLE__
#define NEED_SIGFPE_HANDLER
#define NEED_SIGBUS_HANDLER
#endif

static struct sigaction prev_sigsegv_act;

#ifdef NEED_SIGFPE_HANDLER
static struct sigaction prev_sigfpe_act;
#endif
#ifdef NEED_SIGBUS_HANDLER
static struct sigaction prev_sigbus_act;
#endif

static void
get_bt_from_ucontext (void *uctxt, struct backtrace_addrs *bt)
{
  void *pc = NULL;
  int i;

#ifdef HAVE_BACKTRACE
  bt->size = backtrace (bt->addrs, sizeof (bt->addrs) / sizeof (void *));
  bt->skip = 0;
#else
  bt->size = 0;
  return;
#endif

#if defined (__linux__) && defined (__x86_64__)
  ucontext_t *u = (ucontext_t *)uctxt;
  pc = (void *)u->uc_mcontext.gregs[REG_RIP];
#endif
#if defined (__linux__) && defined (__i386__)
  ucontext_t *u = (ucontext_t *)uctxt;
  pc = (void *)u->uc_mcontext.gregs[REG_EIP];
#endif
#if defined (__APPLE__) && defined (__i386__)
  ucontext_t *u = (ucontext_t *)uctxt;
  pc = (void *)u->uc_mcontext->__ss.__eip;
  bt->skip = 3;  /* This frame + sighandler + trampoline + marker - pc.  */
  bt->addrs[3] = pc;
  return;
#endif

  for (i = 0; i < bt->size; i++)
    if (bt->addrs[i] == pc)
      {
        bt->skip = i;
        break;
      }
}

/* Handler for SIGFPE signal.
   It is also raised in case of overflow (i386 linux).  */
static void
grt_overflow_handler (int signo, siginfo_t *info, void *ptr)
{
  struct backtrace_addrs bt;

  get_bt_from_ucontext (ptr, &bt);
  grt_overflow_error (&bt);
}

/* Posix handler for overflow.  This is used only by mcode.  */
static void
grt_sigsegv_handler (int signo, siginfo_t *info, void *ptr)
{
  struct backtrace_addrs bt;

  get_bt_from_ucontext (ptr, &bt);

#if defined (__linux__) && (defined (__i386__) || defined (__x86_64__))
  if (signo == SIGSEGV)
    {
      ucontext_t *uctxt = (ucontext_t *)ptr;

      /* Linux generates a SIGSEGV (!) for an overflow exception.  */
      if (uctxt->uc_mcontext.gregs[REG_TRAPNO] == 4)
	grt_overflow_error (&bt);
    }
#endif

  /* We loose.  */
  grt_null_access_error (&bt);
}

static void
grt_signal_setup (void)
{
  {
    struct sigaction sigsegv_act;

    sigsegv_act.sa_sigaction = &grt_sigsegv_handler;
    sigemptyset (&sigsegv_act.sa_mask);
    sigsegv_act.sa_flags = SA_SIGINFO;
#ifdef SA_ONESHOT
    sigsegv_act.sa_flags |= SA_ONESHOT;
#elif defined (SA_RESETHAND)
    sigsegv_act.sa_flags |= SA_RESETHAND;
#endif

    /* We don't care about the return status.
       If the handler is not installed, then some feature are lost.  */
    sigaction (SIGSEGV, &sigsegv_act, &prev_sigsegv_act);

#ifdef NEED_SIGBUS_HANDLER
    sigaction (SIGBUS, &sigsegv_act, &prev_sigbus_act);
#endif
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

static void
grt_signal_restore (void)
{
  sigaction (SIGSEGV, &prev_sigsegv_act, NULL);

#ifdef NEED_SIGBUS_HANDLER
  sigaction (SIGBUS, &prev_sigbus_act, NULL);
#endif

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

void
grt_save_backtrace (struct backtrace_addrs *bt, int skip)
{
#ifdef HAVE_BACKTRACE
  bt->size = backtrace (bt->addrs, sizeof (bt->addrs) / sizeof (void *));
  bt->skip = skip + 1;
#else
  bt->size = 0;
#endif
}
