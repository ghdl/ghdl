#include <stdio.h>
#include <vpi_user.h>

static void
show (vpiHandle h)
{
  s_vpi_value val;
  int type = vpi_get (vpiType, h);

  vpi_printf ("  %s", vpi_get_str (vpiName, h));
  vpi_printf (" (%s)", vpi_get_str (vpiFullName, h));
  vpi_printf (" type=%s size=%d\n",
              type == vpiParameter ? "vpiParameter" : "other",
              vpi_get (vpiSize, h));

  switch (vpi_get (vpiConstType, h)) {
  case vpiRealConst:
    val.format = vpiRealVal;
    vpi_get_value (h, &val);
    vpi_printf ("    real=%g\n", val.value.real);
    break;
  case vpiStringConst:
    val.format = vpiStringVal;
    vpi_get_value (h, &val);
    vpi_printf ("    str=%s\n", val.value.str);
    break;
  default:
    val.format = vpiIntVal;
    vpi_get_value (h, &val);
    vpi_printf ("    int=%d\n", val.value.integer);
    val.format = vpiBinStrVal;
    vpi_get_value (h, &val);
    vpi_printf ("    bin=%s\n", val.value.str);
    break;
  }
}

static void
show_params (vpiHandle scope)
{
  vpiHandle iter, h;

  vpi_printf ("%s:\n", vpi_get_str (vpiFullName, scope));
  iter = vpi_iterate (vpiParameter, scope);
  if (iter == NULL) {
    vpi_printf ("  no vpiParameter iterator\n");
    return;
  }
  while ((h = vpi_scan (iter)) != NULL)
    show (h);
}

static PLI_INT32
start_of_sim_cb (p_cb_data cb_data)
{
  vpiHandle iter, top, sub;

  iter = vpi_iterate (vpiModule, NULL);
  top = vpi_scan (iter);
  show_params (top);

  iter = vpi_iterate (vpiModule, top);
  while ((sub = vpi_scan (iter)) != NULL)
    show_params (sub);

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
