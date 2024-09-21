#include <stdio.h>
#include <vpi_user.h>
#include <stdlib.h>
#include <string.h>

PLI_INT32
vpi_proc2 (s_cb_data *cb)
{
  if (cb->value->format != vpiIntVal || cb->value->value.integer != 17) {
    vpi_printf ("Error: unexpected data in callback: %d/%d\n",
                cb->value->format, cb->value->value.integer);
  }
  if (cb->time->type != vpiScaledRealTime || cb->time->real != 1e-3) {
    vpi_printf ("Error: unexpected time in callback: %d at %g\n",
                cb->value->value.integer, cb->time->real);
  }
  return 0;
}

PLI_INT32
vpi_proc1 (s_cb_data *cb)
{
  const char  port_name[] = "myentity.out1";
  s_vpi_time  time;
  s_vpi_value val;
  s_cb_data   ncb;
  vpiHandle   port;

  port = vpi_handle_by_name ((char *)port_name, NULL);
  if (port == NULL) {
    vpi_printf ("Error: no port %s.\n", port_name);
    return 0;
  }

  time.type = vpiScaledRealTime;
  val.format = vpiIntVal;
  ncb.reason = cbValueChange;
  ncb.obj = port;
  ncb.time = &time;
  ncb.value = &val;
  ncb.cb_rtn = &vpi_proc2;
  if (vpi_register_cb (&ncb) == NULL)
    vpi_printf ("Error: Cannot register cbValueChange call back\n");
  return 0;
}

void my_handle_register()
{
  s_cb_data cb;
  printf ("Hello world\n");

  cb.reason = cbEndOfCompile;
  cb.cb_rtn = &vpi_proc1;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot register EndOfCompile call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
