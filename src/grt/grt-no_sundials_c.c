#include <stddef.h>
typedef double realtype;

realtype *
grt__sundials__get_yy_vec(void)
{
  return NULL;
}

realtype *
grt__sundials__get_yp_vec(void)
{
  return NULL;
}

int *
grt__sundials__get_root_info(void)
{
  return NULL;
}

int
grt__sundials__create(int sz, int nbr_roots)
{
  return -1;
}

void grt__sundials__set_algebric_variable(int idx)
{
}

void grt__sundials__set_differential_variable(int idx)
{
}

int
grt__sundials__initial_conditions(realtype tout1)
{
  return -1;
}

void
grt__sundials__set_tolerances_scalar (realtype atol, realtype rtol)
{
}


int
grt__sundials__reinit(realtype t)
{
  return -1;
}

int
grt__sundials__initialize (void)
{
  return -1;
}

void grt__sundials__set_max_step (realtype hmax)
{
}

void
grt__sundials__solve(realtype t0, realtype *tn, int *res)
{
  *res = 2;
}
