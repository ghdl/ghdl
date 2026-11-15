#include <stdio.h>
#include <vpi_user.h>

static const char *
fullname (vpiHandle h)
{
  return h ? vpi_get_str (vpiFullName, h) : "(null)";
}

static void
show_ports (vpiHandle scope)
{
  vpiHandle iter, port, sub;

  vpi_printf ("%s:\n", vpi_get_str (vpiFullName, scope));
  iter = vpi_iterate (vpiPort, scope);
  while (iter != NULL && (port = vpi_scan (iter)) != NULL) {
    vpiHandle low = vpi_handle (vpiLowConn, port);
    vpiHandle high = vpi_handle (vpiHighConn, port);
    vpi_printf ("  port %s lowConn=%s", vpi_get_str (vpiName, port),
                fullname (low));
    vpi_printf (" highConn=%s\n", fullname (high));
  }

  iter = vpi_iterate (vpiModule, scope);
  while (iter != NULL && (sub = vpi_scan (iter)) != NULL)
    show_ports (sub);
}

static PLI_INT32
start_of_sim_cb (p_cb_data cb_data)
{
  vpiHandle iter = vpi_iterate (vpiModule, NULL);
  show_ports (vpi_scan (iter));
  return 0;
}

static void
my_handle_register (void)
{
  s_cb_data cb;

  cb.reason = cbStartOfSimulation;
  cb.cb_rtn = start_of_sim_cb;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register StartOfSimulation call-back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
