#include <stdio.h>
#include <string.h>
#include <vpi_user.h>

static vpiHandle sig_b;

static PLI_INT32
cb_delay(p_cb_data data)
{
  s_vpi_value val;
  val.format = vpiBinStrVal;

  vpi_get_value (sig_b, &val);
  printf("b = %s\n", val.value.str);

  return 0;
}

static void
reg_delay(unsigned cyc)
{
  s_cb_data cbData;
  s_vpi_time simuTime;

  cbData.time = &simuTime;
  simuTime.type = vpiSimTime;
  simuTime.high = 0;
  simuTime.low = cyc;

  cbData.reason = cbAfterDelay;
  cbData.cb_rtn = cb_delay;
  cbData.user_data = 0;
  cbData.value = 0;

  if (vpi_register_cb(&cbData) == NULL)
    vpi_printf ("cannot register ValueChange call back\n");
}

static PLI_INT32
vpi_start_proc(p_cb_data data)
{
  s_vpi_value val;
  s_cb_data cb;

  sig_b = vpi_handle_by_name ("mwe.b", NULL);
  if (sig_b == NULL) {
    printf ("cannot get signal 'b'\n");
    return 0;
  }

  val.format = vpiBinStrVal;
  val.value.str = "1";
  vpi_put_value (sig_b, &val, NULL, vpiInertialDelay);
  val.format = vpiBinStrVal;
  val.value.str = "0";
  vpi_put_value (sig_b, &val, NULL, vpiInertialDelay);

  reg_delay(10000000);

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
    vpi_printf ("cannot register StartOfSimulation call-back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
