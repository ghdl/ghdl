#include <stdio.h>
#include <vpi_user.h>

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
  vpiHandle iter, top, sig_i;

  iter = vpi_iterate (vpiModule, NULL);
  top = vpi_scan (iter);

  sig_i = vpi_handle_by_name ("sig_i", top);

  /* sig_i: t_sulv_vector, unresolved element -- the crashing case. Prior
     to the fix, this call to vpi_handle_by_index aborts the whole
     simulator with "internal error: add_index(2)" before ever returning
     here. */
  read_el (sig_i, "sig_i", 0);
  read_el (sig_i, "sig_i", 1);

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
