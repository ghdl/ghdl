#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vpi_user.h>

static vpiHandle clk;
static unsigned cycle;

static void
show_value (const char *name)
{
  vpiHandle net;
  vpiHandle rng;
  vpiHandle el;
  s_vpi_value val;
  int left, right;
  int i;

  net = vpi_handle_by_name ((char *)name, NULL);
  if (net == NULL)
    {
      printf ("cannot get net: %s\n", name);
      exit(1);
      return;
    }

  /* Get range.  */
  rng = vpi_handle (vpiLeftRange, net);
  val.format = vpiIntVal;
  vpi_get_value (rng, &val);
  left = val.value.integer;

  rng = vpi_handle (vpiRightRange, net);
  val.format = vpiIntVal;
  vpi_get_value (rng, &val);
  right = val.value.integer;

  printf ("%s[%d:%d]\n", name, left, right);

  i = left;
  while (1) {
    el = vpi_handle_by_index (net, i);
    if (el == NULL)
      val.value.str = "ERROR";
    else {
      val.format = vpiBinStrVal;
      vpi_get_value (el, &val);
    }
    printf ("%s[%d] = %s", name, i, val.value.str);
    if (el != NULL) {
      val.format = vpiIntVal;
      vpi_get_value (el, &val);
      printf (" = %d", val.value.integer);
    }
    putchar('\n');
    if (el == NULL || val.value.integer != i)
      exit (1);
    if (i == right)
      break;
    if (left < right)
      i++;
    else
      i--;
  }

#if 0
  val.format = vpiBinStrVal;
  vpi_get_value (net, &val);
  printf ("%s= %s\n", name, val.value.str);
#endif
}

static PLI_INT32
vpi_clk_proc (struct t_cb_data *cb)
{
  s_vpi_value val;

  val.format = vpiBinStrVal;
  vpi_get_value (clk, &val);
  /* Detect edge.  */
  if (strcmp (val.value.str, "1") != 0)
    return 0;

  cycle++;
  printf ("clock cycle %u\n", cycle);

  if (cycle == 2)
    show_value ("test_array.mem_down");

  return 0;
}

static PLI_INT32
endofcompile_proc (struct t_cb_data *compile_cb)
{
  s_cb_data cb;

  clk = vpi_handle_by_name ("test_array.clk", NULL);
  if (clk == NULL)
    {
      printf ("cannot get net clk\n");
      return 0;
    }

  cb.reason = cbValueChange;
  cb.cb_rtn = &vpi_clk_proc;
  cb.user_data = NULL;
  cb.obj = clk;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register ValueChange call back\n");

  cycle = 0;

  return 0;
}

static void my_handle_register(void)
{
  s_cb_data cb;

  cb.reason = cbEndOfCompile;
  cb.cb_rtn = &endofcompile_proc;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register EndOfCompile call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
