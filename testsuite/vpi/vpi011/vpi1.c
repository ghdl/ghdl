#include <stdio.h>
#include <string.h>
#include <vpi_user.h>

static vpiHandle q;
static int cnt;

static void reg_delay(unsigned cyc);

static const char *expected[2] = {"01000000", "10111111"};

static PLI_INT32
cb_delay(p_cb_data data)
{
  vpiHandle e;
  s_vpi_value val;
  unsigned i;

  for (i = 0; i < 2; i++) {
    e = vpi_handle_by_index(q, i);
    val.format = vpiBinStrVal;
    vpi_get_value (e, &val);
    printf("mem[%u] = %s\n", i, val.value.str);
    if (strcmp(val.value.str, expected[i]))
      printf ("ERROR!, expected '%s'\n", expected[i]);
  }

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
  int i;

  q = vpi_handle_by_name ("dut.mem", NULL);
  if (q == NULL) {
    printf ("cannot get net 'mem'\n");
    return 0;
  }
  
  cnt = 0;

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
