#include <stddef.h>
typedef double realtype;

realtype *
grt_sundials_get_yy_vec(void)
{
  return NULL;
}

realtype *
grt_sundials_get_yp_vec(void)
{
  return NULL;
}

int
grt_sundials_init(int sz)
{
  return -1;
}

int
grt_sundials_start (void)
{
  return -1;
}

int
grt_sundials_solve(realtype t0, realtype *tn, int *res)
{
  return 1;
}
