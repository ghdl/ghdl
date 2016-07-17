#include <stdio.h>
#include <vpi_user.h>

void
vpi_proc (void)
{
  vpiHandle net;
  s_vpi_value val;

  net = vpi_handle_by_name ("test_load.dat_o", NULL);
  if (net == NULL)
    {
      printf ("cannot get net\n");
      return;
    }
  val.format = vpiBinStrVal;
  vpi_get_value (net, &val);
  printf ("value: %s\n", val.value.str);
}

void my_handle_register()
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
