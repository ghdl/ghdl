#include <stdio.h>
#include <string.h>
#include <vpi_user.h>

static vpiHandle do_el0, do_el1;
static char expected[8];

static PLI_INT32
cb_delay (p_cb_data cb_data)
{
  s_vpi_value val;

  val.format = vpiBinStrVal;
  vpi_get_value (do_el0, &val);
  vpi_printf ("sig_o(0) after write = %s\n", val.value.str);

  /* sig_o(1) must be untouched by the sig_o(0) write -- a wrong element
     stride would show up here instead. */
  val.format = vpiBinStrVal;
  vpi_get_value (do_el1, &val);
  vpi_printf ("sig_o(1) after write = %s\n", val.value.str);

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

/* Read and print vpiSize + value for one element of an array-of-
   unconstrained-vector port. Returns the element handle (or NULL). */
static vpiHandle
read_el (vpiHandle arr, const char *name, int idx)
{
  vpiHandle el;
  s_vpi_value val;

  el = vpi_handle_by_index (arr, idx);
  if (el == NULL) {
    vpi_printf ("ERROR: vpi_handle_by_index (%s, %d) returned NULL\n", name, idx);
    return NULL;
  }
  val.format = vpiBinStrVal;
  vpi_get_value (el, &val);
  vpi_printf ("%s(%d) size=%d value=%s\n", name, idx, vpi_get (vpiSize, el), val.value.str);
  return el;
}

static PLI_INT32
start_of_sim_cb (p_cb_data cb_data)
{
  vpiHandle iter, top, sig_o;
  s_vpi_value val;

  iter = vpi_iterate (vpiModule, NULL);
  top = vpi_scan (iter);

  sig_o = vpi_handle_by_name ("sig_o", top);

  /* sig_o: t_slv_vector, resolved element -- the silent-NULL case. */
  do_el0 = read_el (sig_o, "sig_o", 0);
  do_el1 = read_el (sig_o, "sig_o", 1);
  if (do_el0 == NULL || do_el1 == NULL)
    return 0;

  /* Write sig_o(0), checked one time unit later: a vpiNoDelay put is not
     guaranteed visible to a get in the same callback before simulation
     time advances. */
  strcpy (expected, "11010");
  val.format = vpiBinStrVal;
  val.value.str = expected;
  vpi_put_value (do_el0, &val, NULL, vpiNoDelay);

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
