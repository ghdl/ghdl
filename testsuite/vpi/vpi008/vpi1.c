/* Test for vpiVectorVal. */

#include <stdio.h>
#include <vpi_user.h>
#include <stdlib.h>
#include <string.h>

static s_vpi_time   when; // Structures are re-used.
static s_cb_data    cb;
static s_vpi_vecval vec;
static s_vpi_value  val;
static vpiHandle    port;
static char         buf[10];

static void vec_to_text (s_vpi_vecval *vp, char *bp)
{
  int i;

  for (i = 0; i < 9; ++i) {
    if (vp->bval & 1)
      bp[i] = (vp->aval & 1) ? 'X' : 'Z';
    else
      bp[i] = '0' + (vp->aval & 1);
    vp->aval >>= 1;
    vp->bval >>= 1;
  }
  bp[i] = '\0';
}

static PLI_INT32
vpi_proc3 (s_cb_data *pcb)
{
  vpi_printf ("Error: Cancelled callback called.\n");
  return 0;
}

static PLI_INT32
vpi_proc2 (s_cb_data *pcb)
{
  vpiHandle h1, h2, h3;

  vpi_get_value (port, &val);
  vpi_printf ("Got %#x %#x\n", vec.aval, vec.bval);
  vec_to_text (&vec, buf);
  vpi_printf ("Translation of round-trip Verilog \"1X0Z1X0Z0\": %s\n", buf);
  if (strcmp(buf, "1X0Z1X0Z0")) {
    vpi_printf ("Error: Got %s but expected \"1X0Z1X0Z0\" for round trip.\n",
                buf);
  }

  /* Test callback cancellation. */

  cb.cb_rtn = &vpi_proc3;
  h1 = vpi_register_cb (&cb);
  when.low = 1200;
  h3 = vpi_register_cb (&cb);
  when.low = 1100;
  h2 = vpi_register_cb (&cb);
  if (h1 == NULL || h2 == NULL || h3 == NULL)
    vpi_printf ("Error: Cannot register extra AfterDelay call backs.\n");
  vpi_remove_cb (h3);
  vpi_remove_cb (h2);
  vpi_remove_cb (h1);
  return 0;
}

static PLI_INT32
vpi_proc1 (s_cb_data *pcb)
{
  const char   port_name[] = "myentity.inout1";

  port = vpi_handle_by_name ((char *)port_name, NULL);
  if (port == NULL) {
    vpi_printf ("Error: no port %s.\n", port_name);
    return 0;
  }
 
  val.format = vpiVectorVal;
  val.value.vector = &vec;
  vpi_get_value (port, &val);
  vpi_printf ("Got %#x %#x\n", vec.aval, vec.bval);
  vec_to_text (&vec, buf);
  vpi_printf ("Translation of VHDL \"10ZWLH-UX\": %s\n", buf);
  if (strcmp(buf, "10ZX01XXX")) {
    vpi_printf ("Error: Got %s but expected \"10ZX01XXX\" for translation of "
                "VHDL \"10ZWLH-UX\"\n", buf);
  }

  vec.aval = 0x33;
  vec.bval = 0xaa;
  vpi_put_value (port, &val, NULL, vpiNoDelay); // Effect is not immediate.
  cb.cb_rtn = &vpi_proc2;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot re-register AfterDelay call back\n");
  return 0;
}

void my_handle_register()
{
  printf ("Hello world\n");

  when.type = vpiSimTime;
  when.low = 1000;
  when.high = 0;
  cb.reason = cbAfterDelay;
  cb.time = &when;
  cb.cb_rtn = &vpi_proc1;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot register AfterDelay call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
