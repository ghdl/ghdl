#include <stdio.h>
#include <string.h>
#include <vpi_user.h>

static PLI_INT32
vpi_start_proc(p_cb_data data)
{
  vpi_control(vpiFinish, 0);

  return 0;
}

static void
my_handle_register(void)
{
  s_cb_data cb;

  printf ("register vpi...\n");
  cb.reason = cbStartOfSimulation;
  cb.cb_rtn = &vpi_start_proc;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register StartOfSimulation call-back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
