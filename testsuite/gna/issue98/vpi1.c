#include <stdio.h>
#include <vpi_user.h>

PLI_INT32
vpi_proc (s_cb_data *cb)
{
  vpiHandle net;
  s_vpi_value val;

  net = vpi_handle_by_name ("test_load.dat_o", NULL);
  if (net == NULL)
    {
      printf ("cannot get net\n");
      return -1;
    }
  val.format = vpiBinStrVal;
  vpi_get_value (net, &val);
  printf ("value: %s\n", val.value.str);
  return 0;
}

void my_handle_register()
{
  s_cb_data cb;

  cb.reason = cbEndOfCompile;
  cb.cb_rtn = &vpi_proc;
  cb.user_data = NULL;
  cb.time = NULL;
  cb.value = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register EndOfCompile call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
