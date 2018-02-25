#include <stdio.h>
#include <string.h>
#include <vpi_user.h>

struct net_descs
{
  const char *name;
  vpiHandle *handle;
};

static vpiHandle clk, arg_a, arg_b, res;
static int cnt;

static struct net_descs nets[] = {
  { "sliced_ex.clk", &clk},
  { "sliced_ex.arg_a", &arg_a},
  { "sliced_ex.arg_b", &arg_b},
  { "sliced_ex.sub_module.res", &res},
  { NULL, NULL}
};

static PLI_INT32
vpi_clk_proc(p_cb_data data)
{
  s_vpi_value val;

  val.format = vpiBinStrVal;
  vpi_get_value (clk, &val);
  /* Detect edge.  */
  if (strcmp (val.value.str, "1") != 0)
    return 0;

  val.format = vpiBinStrVal;
  vpi_get_value (res, &val);
  printf ("cycle %d: res = %s\n", cnt, val.value.str);

  switch (cnt)
    {
    case 0:
      val.format = vpiBinStrVal;
      val.value.str = "0001";
      vpi_put_value (arg_a, &val, NULL, vpiNoDelay);
      val.format = vpiBinStrVal;
      val.value.str = "0010";
      vpi_put_value (arg_b, &val, NULL, vpiNoDelay);
      break;
    case 2:
      val.format = vpiBinStrVal;
      val.value.str = "0010";
      vpi_put_value (arg_a, &val, NULL, vpiNoDelay);
      val.format = vpiBinStrVal;
      val.value.str = "0011";
      vpi_put_value (arg_b, &val, NULL, vpiNoDelay);
      break;
    case 3:
      if (strcmp(val.value.str, "00000000") != 0)
	printf ("Error!\n");
      break;
    case 4:
      if (strcmp(val.value.str, "00010011") != 0)
	printf ("Error!\n");
      break;
    default:
      break;
    }

  cnt++;
  return 0;
}

static PLI_INT32
vpi_start_proc(p_cb_data data)
{
  s_vpi_value val;
  s_cb_data cb;
  int i;

  for (i = 0; nets[i].name; i++)
    {
      *nets[i].handle = vpi_handle_by_name ((char *)nets[i].name, NULL);
      if (*nets[i].handle == NULL)
	{
	  printf ("cannot get net %s\n", nets[i].name);
	  return 0;
	}
    }

  cb.reason = cbValueChange;
  cb.cb_rtn = &vpi_clk_proc;
  cb.user_data = NULL;
  cb.obj = clk;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register ValueChange call back\n");

  return 0;
}

static void
my_handle_register(void)
{
  s_cb_data cb;

  cb.reason = cbStartOfSimulation;
  cb.cb_rtn = &vpi_start_proc;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register EndStartOfSimulation call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
