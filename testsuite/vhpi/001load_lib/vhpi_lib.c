#include <stdio.h>
#include <vhpi_user.h>

void my_startup()
{
  printf ("VHPI lib\n");
}

void (*vhpi_startup_routines[]) () =
{
  my_startup,
  0
};
