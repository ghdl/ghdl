#include <stdio.h>

#ifdef SECOND
  printf ("second\n");
}

int main(void)
{
  func();
}
#endif
#ifdef FIRST
int func(void)
{
  printf ("first\n");
#endif

#if !defined(FIRST) && !defined(SECOND)
#define FIRST
#include "repro2.c"
#undef FIRST
#define SECOND
#include "repro2.c"
#endif
