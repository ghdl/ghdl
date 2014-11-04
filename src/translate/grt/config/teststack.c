#include <stdlib.h>
#include <stdio.h>

extern void grt_stack_init (void);
extern void grt_stack_switch (void *from, void *to);
extern void *grt_stack_create (void (*func)(void *), void *arg);

int stack_size = 4096;
int stack_max_size = 8 * 4096;

static void *stack1;
static void *stack2;
void *grt_stack_main_stack;

void *grt_cur_proc;

static int step;

void
grt_overflow_error (void)
{
  abort ();
}

void
grt_stack_error_null_access (void)
{
  abort ();
}

void
grt_stack_error_memory_access (void)
{
  abort ();
}

void
grt_stack_error_grow_failed (void)
{
  abort ();
}

void
error (void)
{
  printf ("Test failure at step %d\n", step);
  fflush (stdout);
  exit (1);
}

static void
func1 (void *ptr)
{
  if (ptr != (void *)1)
    error ();

  if (step != 0)
    error ();

  step = 1;

  grt_stack_switch (grt_stack_main_stack, stack1);

  if (step != 5)
    error ();

  step = 6;

  grt_stack_switch (grt_stack_main_stack, stack1);

  if (step != 7)
    error ();

  step = 8;

  grt_stack_switch (stack2, stack1);

  if (step != 9)
    error ();

  step = 10;

  grt_stack_switch (grt_stack_main_stack, stack1);

  error ();
}

static void
func2 (void *ptr)
{
  if (ptr != (void *)2)
    error ();

  if (step == 11)
    {
      step = 12;

      grt_stack_switch (grt_stack_main_stack, stack2);

      error ();
    }

  if (step != 1)
    error ();

  step = 2;

  grt_stack_switch (grt_stack_main_stack, stack2);

  if (step != 3)
    error ();

  step = 4;

  grt_stack_switch (grt_stack_main_stack, stack2);

  if (step != 8)
    error ();

  step = 9;

  grt_stack_switch (stack1, stack2);
}

int
main (void)
{
  grt_stack_init ();

  stack1 = grt_stack_create (&func1, (void *)1);
  stack2 = grt_stack_create (&func2, (void *)2);

  step = 0;
  grt_stack_switch (stack1, grt_stack_main_stack);

  if (step != 1)
    error ();

  grt_stack_switch (stack2, grt_stack_main_stack);

  if (step != 2)
    error ();

  step = 3;

  grt_stack_switch (stack2, grt_stack_main_stack);

  if (step != 4)
    error ();

  step = 5;

  grt_stack_switch (stack1, grt_stack_main_stack);

  if (step != 6)
    error ();

  step = 7;

  grt_stack_switch (stack1, grt_stack_main_stack);

  if (step != 10)
    error ();

  step = 11;

  grt_stack_switch (stack2, grt_stack_main_stack);

  if (step != 12)
    error ();

  printf ("Test successful\n");
  return 0;
}
