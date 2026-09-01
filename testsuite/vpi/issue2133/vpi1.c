/*  Deposit a value into an INOUT std_logic_vector port from VPI, which is
    what cocotb does for "dut.MC_AD.value = 0xFAFA" in issue #2133.  That
    used to end in

      raised CONSTRAINT_ERROR : grt-signals.adb:3522 access check failed

    Read the port back one time unit later so a wrong deposit shows up as a
    value mismatch rather than only as the absence of a crash.  */

#include <stdio.h>
#include <string.h>
#include <vpi_user.h>

static vpiHandle mc_ad, ad_out;
static char oe_val[4];
static char bus_val[20];

static PLI_INT32
cb_delay (p_cb_data cb_data)
{
  s_vpi_value val;

  val.format = vpiBinStrVal;
  vpi_get_value (mc_ad, &val);
  vpi_printf ("mc_ad after deposit = %s\n", val.value.str);

  val.format = vpiBinStrVal;
  vpi_get_value (ad_out, &val);
  vpi_printf ("ad_out after deposit = %s\n", val.value.str);

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
  vpiHandle iter, top, oe_ad;
  s_vpi_value val;

  iter = vpi_iterate (vpiModule, NULL);
  top = vpi_scan (iter);

  oe_ad = vpi_handle_by_name ("oe_ad", top);
  mc_ad = vpi_handle_by_name ("mc_ad", top);
  ad_out = vpi_handle_by_name ("ad_out", top);

  if (oe_ad == NULL || mc_ad == NULL || ad_out == NULL)
    {
      vpi_printf ("ERROR: cannot get a handle on the ports\n");
      return 0;
    }

  /*  The design is not driving the bus: cocotb's first statement.  */
  strcpy (oe_val, "0");
  val.format = vpiBinStrVal;
  val.value.str = oe_val;
  vpi_put_value (oe_ad, &val, NULL, vpiNoDelay);

  /*  Deposit into the INOUT port -- 0xFAFA, as in the report.  */
  strcpy (bus_val, "1111101011111010");
  val.format = vpiBinStrVal;
  val.value.str = bus_val;
  vpi_put_value (mc_ad, &val, NULL, vpiNoDelay);

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
