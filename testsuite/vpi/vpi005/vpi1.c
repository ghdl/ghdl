#include <stdio.h>
#include <vpi_user.h>

void my_startup()
{
  printf ("VPI lib 1\n");
}

void (*vlog_startup_routines[]) () =
{
  my_startup,
  0
};
