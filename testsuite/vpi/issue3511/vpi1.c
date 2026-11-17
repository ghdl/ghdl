#include <stdio.h>
#include <vpi_user.h>

/* Issue #3511: a cbReadOnlySynch callback registered at the rising edge of
   clk must not apply the pending transaction of x before its time.  */

static vpiHandle clk;
static vpiHandle x;

static unsigned int
now_fs (void)
{
  s_vpi_time t;

  t.type = vpiSimTime;
  vpi_get_time (NULL, &t);
  return t.low;
}

static const char *
value_of (vpiHandle h)
{
  static s_vpi_value val;

  val.format = vpiBinStrVal;
  vpi_get_value (h, &val);
  return val.value.str;
}

static PLI_INT32
readonly_cb (p_cb_data cb_data)
{
  vpi_printf ("cbReadOnlySynch at %u fs: x=%s\n", now_fs (), value_of (x));
  return 0;
}

static PLI_INT32
clk_cb (p_cb_data cb_data)
{
  s_cb_data cb;
  s_vpi_time t;

  if (value_of (clk)[0] != '1')
    return 0;

  vpi_printf ("clk rising at %u fs\n", now_fs ());

  t.type = vpiSimTime;
  t.high = 0;
  t.low = 0;
  cb.reason = cbReadOnlySynch;
  cb.cb_rtn = readonly_cb;
  cb.obj = NULL;
  cb.time = &t;
  cb.value = NULL;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register ReadOnlySynch call-back\n");
  return 0;
}

static PLI_INT32
x_cb (p_cb_data cb_data)
{
  vpi_printf ("x changed to %s at %u fs\n", value_of (x), now_fs ());
  return 0;
}

static void
register_value_change (vpiHandle h, PLI_INT32 (*rtn) (p_cb_data))
{
  static s_vpi_time t;
  static s_vpi_value v;
  s_cb_data cb;

  t.type = vpiSuppressTime;
  v.format = vpiBinStrVal;
  cb.reason = cbValueChange;
  cb.cb_rtn = rtn;
  cb.obj = h;
  cb.time = &t;
  cb.value = &v;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register ValueChange call-back\n");
}

static PLI_INT32
start_of_sim_cb (p_cb_data cb_data)
{
  clk = vpi_handle_by_name ("myentity.clk", NULL);
  x = vpi_handle_by_name ("myentity.x", NULL);
  if (clk == NULL || x == NULL)
    {
      vpi_printf ("cannot find myentity.clk or myentity.x\n");
      return 0;
    }
  register_value_change (clk, clk_cb);
  register_value_change (x, x_cb);
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
