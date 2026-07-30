#include <stdio.h>
#include <vpi_user.h>

static vpiHandle do_el;

static PLI_INT32
cb_delay (p_cb_data cb_data)
{
  s_vpi_value val;

  val.format = vpiIntVal;
  vpi_get_value (do_el, &val);
  vpi_printf ("sig_o(0) = %d\n", val.value.integer);

  return 0;
}

static void
reg_delay (unsigned cyc)
{
  s_cb_data cb;
  s_vpi_time delay;

  delay.type = vpiSimTime;
  delay.high = 0;
  delay.low = cyc;

  cb.reason = cbAfterDelay;
  cb.cb_rtn = cb_delay;
  cb.time = &delay;
  cb.user_data = NULL;

  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register AfterDelay call-back\n");
}

static PLI_INT32
start_of_sim_cb (p_cb_data cb_data)
{
  vpiHandle iter, top, sig_i, sig_o, el;
  s_vpi_value val;
  val.format = vpiIntVal;

  iter = vpi_iterate (vpiModule, NULL);
  top = vpi_scan (iter);

  sig_i = vpi_handle_by_name ("sig_i", top);
  sig_o = vpi_handle_by_name ("sig_o", top);

  //----------------------------
  // Read sig_i value from VHDL
  //----------------------------
  el = vpi_handle_by_index (sig_i, 0);
  if (el == NULL) {
    vpi_printf ("ERROR: vpi_handle_by_index (sig_i, 0) returned NULL\n");
    return 0;
  }
  vpi_get_value (el, &val);
  vpi_printf ("sig_i(0) = %d\n", val.value.integer);

  el = vpi_handle_by_index (sig_i, 1);
  if (el == NULL) {
    vpi_printf ("ERROR: vpi_handle_by_index (sig_i, 1) returned NULL\n");
    return 0;
  }
  vpi_get_value (el, &val);
  vpi_printf ("sig_i(1) = %d\n", val.value.integer);

  //----------------------------
  // Read sig_o value from VHDL
  //----------------------------
  el = vpi_handle_by_index (sig_o, 0);
  if (el == NULL) {
    vpi_printf ("ERROR: vpi_handle_by_index (sig_o, 0) returned NULL\n");
    return 0;
  }
  vpi_get_value (el, &val);
  vpi_printf ("sig_o(0) = %d\n", val.value.integer);

  el = vpi_handle_by_index (sig_o, 1);
  if (el == NULL) {
    vpi_printf ("ERROR: vpi_handle_by_index (sig_o, 1) returned NULL\n");
    return 0;
  }
  vpi_get_value (el, &val);
  vpi_printf ("sig_o(1) = %d\n", val.value.integer);

  //----------------------------
  // Write sig_o value
  //----------------------------
  do_el = vpi_handle_by_index (sig_o, 0);
  if (do_el == NULL) {
    vpi_printf ("ERROR: vpi_handle_by_index (sig_o, 0) returned NULL\n");
    return 0;
  }
  val.value.integer = 99;
  vpi_put_value (do_el, &val, NULL, vpiNoDelay);

  reg_delay (1);

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
