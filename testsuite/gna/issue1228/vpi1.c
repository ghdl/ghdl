#include <stdio.h>
#include <stdlib.h>
#include <vpi_user.h>

static void
show_value (const char *name)
{
  vpiHandle net;
  s_vpi_value val;

  net = vpi_handle_by_name ((char *)name, NULL);
  if (net == NULL)
    {
      printf ("cannot get net: %s\n", name);
      exit(1);
      return;
    }
  val.format = vpiBinStrVal;
  vpi_get_value (net, &val);
  printf ("%s= %s\n", name, val.value.str);
}

static PLI_INT32
vpi_proc (struct t_cb_data *cb)
{
  show_value ("test_load.w");
  show_value ("test_load.\\extend.id\\");
  return 0;
}

static void my_handle_register(void)
{
  s_cb_data cb;

  cb.reason = cbEndOfCompile;
  cb.cb_rtn = &vpi_proc;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register EndOfCompile call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
