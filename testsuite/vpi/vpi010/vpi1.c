#include <stdio.h>
#include <string.h>
#include <vpi_user.h>

static vpiHandle q;
static int cnt;

static void reg_delay(unsigned cyc);

static const char expected[] = "?00001100000";

static PLI_INT32
cb_delay(p_cb_data data)
{
  s_vpi_time now;
  s_vpi_value val;

  cnt++;
  
  now.type = vpiScaledRealTime;
  vpi_get_time(NULL, &now);

  val.format = vpiBinStrVal;
  vpi_get_value (q, &val);


  printf("callback at %f ns, cnt=%u, q=%s\n",
	 now.real * 1e9, cnt, val.value.str);

  if (val.value.str[0] != expected[cnt])
    printf ("ERROR!, expected '%c'\n", expected[cnt]);
  
  if (cnt == 6 || cnt == 8) {
      val.format = vpiBinStrVal;
      val.value.str = "0";
      vpi_put_value (q, &val, NULL, vpiNoDelay);
  }
  else if (cnt == 7) {
      val.format = vpiBinStrVal;
      val.value.str = "0";
      vpi_put_value (q, &val, NULL, vpiForceFlag);
  }

  if (cnt < 10)
    reg_delay(10000000);

#if 0
  /* Detect edge.  */
  if (strcmp (val.value.str, "1") != 0)
    return 0;

  val.format = vpiBinStrVal;
  vpi_get_value (res, &val);
  printf ("cycle %d: res = %s\n", cnt, val.value.str);

  switch (cnt)
    {
    case 0:
      val.format = vpiBinStrVal;
      val.value.str = "0001";
      vpi_put_value (v_32, &val, NULL, vpiNoDelay);
      val.format = vpiBinStrVal;
      val.value.str = "0010";
      vpi_put_value (v_8, &val, NULL, vpiNoDelay);
      break;
    case 2:
      val.format = vpiBinStrVal;
      val.value.str = "0010";
      vpi_put_value (v_32, &val, NULL, vpiNoDelay);
      val.format = vpiBinStrVal;
      val.value.str = "0011";
      vpi_put_value (v_8, &val, NULL, vpiNoDelay);
      break;
    case 3:
      if (strcmp(val.value.str, "00000000000000000000000000000101") != 0)
	printf ("Error!\n");
      val.format = vpiIntVal;
      val.value.integer = 123;
      vpi_put_value (v_32, &val, NULL, vpiNoDelay);
      break;
    case 4:
      if (strcmp(val.value.str, "00000000000000000000000001111110") != 0)
	printf ("Error!\n");
      break;
    default:
      break;
    }

  cnt++;
  #endif
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

  q = vpi_handle_by_name ("repro1.q", NULL);
  if (q == NULL) {
    printf ("cannot get net 'q'\n");
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
