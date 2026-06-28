/* Test for vpiRealVal and vpiScalarVal. */

#include <stdio.h>
#include <vpi_user.h>
#include <stdlib.h>
#include <string.h>

static const char   letters[] =  "01ZXHL-!";
static const char   std_lets[] = "10ZWLH-UX";
static const char   std_exp[] =  "10ZXLH-XX";
static s_vpi_time   when; // Structures are re-used.
static s_cb_data    cb;
static s_vpi_vecval vec;
static s_vpi_value  val;
static vpiHandle    rport, lport, cbh;
static int          i;
static char         buf[10];

static void rval_error(double got, double wanted)
{
  vpi_printf ("Error: port inr value was %g when %g expected.\n",
              got, wanted);
}

static void sval_error(unsigned int got, unsigned int wanted)
{
  if (got > vpiDontCare)
      got = vpiDontCare + 1;
  vpi_printf ("Error: port inl scalar value was '%c' when '%c' expected.\n",
              letters[got], letters[wanted]);
}

/* Run through the vpiScalarVal values.  VHDL will check translations. */

static PLI_INT32
value_cb2 (s_cb_data *pcb)
{
  val.format = vpiScalarVal;
  val.value.scalar = i;
  vpi_put_value (lport, &val, NULL, vpiNoDelay);
  if (++i > 7)
    vpi_remove_cb (cbh); // Cancel this callback.
  return 0;
}

/* Check translations as VHDL runs through the std_logic values. */

static PLI_INT32
value_cb (s_cb_data *pcb)
{
  val.format = vpiScalarVal;
  vpi_get_value (lport, &val);
  if (letters[val.value.scalar] != std_exp[i])
    vpi_printf("Error: std_logic %c translated to %c but %c was expected.\n",
               std_lets[i], letters[val.value.scalar], std_exp[i]);
  if (++i > 8) {
    /* Changes to the real port will ba a signal to change the logic port. */

    i = 0;
    vpi_remove_cb (cbh); // Cancel this callback.
    cb.value = NULL;
    cb.obj = rport;
    cb.cb_rtn = value_cb2;
    cbh = vpi_register_cb (&cb);
    if (cbh == NULL)
      vpi_printf ("Error: Cannot register ValueChange call back 2.\n");
  }
  return 0;
}

static PLI_INT32
next_cb (s_cb_data *pcb)
{
  static char *vport_name = "myentity.holder";
  s_vpi_value  val;

  val.format = vpiRealVal;
  vpi_get_value (rport, &val);
  if (val.value.real != 0.015)
      rval_error(val.value.real, 0.015);

  val.format = vpiScalarVal;
  vpi_get_value (lport, &val);
  if (val.value.scalar != vpiH)
      sval_error(val.value.scalar, vpiH);

  /* Set a value change callback to be triggered by VHDL and
   * check translations.
   */
  
  cb.reason = cbValueChange;
  cb.cb_rtn = value_cb;
  val.format = vpiScalarVal;
  cb.value = &val;
  cb.obj = lport;
  cb.cb_rtn = value_cb;
  cbh = vpi_register_cb (&cb);
  if (cbh == NULL)
      vpi_printf ("Error: Cannot register ValueChange call back 1.\n");
  return 0;
}

static PLI_INT32
start_cb (s_cb_data *pcb)
{
  static char *rport_name = "myentity.inr";
  static char *lport_name = "myentity.inl";
  s_vpi_value  val;

  rport = vpi_handle_by_name (rport_name, NULL);
  if (rport == NULL) {
    vpi_printf ("Error: no port %s.\n", rport_name);
    return 0;
  }
  lport = vpi_handle_by_name (lport_name, NULL);
  if (rport == NULL) {
    vpi_printf ("Error: no port %s.\n", lport_name);
    return 0;
  }

  val.format = vpiRealVal;
  vpi_get_value (rport, &val);
  if (val.value.real != -4.2)
      rval_error(val.value.real, 4.2);
  val.value.real = 0.015;
  vpi_put_value (rport, &val, NULL, vpiNoDelay); // Effect is not immediate.

  val.format = vpiScalarVal;
  vpi_get_value (lport, &val);
  if (val.value.scalar != vpiX)
      sval_error(val.value.scalar, vpiX);
  val.value.scalar = vpiH;
  vpi_put_value (lport, &val, NULL, vpiNoDelay); // Effect is not immediate.

  when.type = vpiSimTime;
  when.low = 1000;
  when.high = 0;
  cb.reason = cbAfterDelay;
  cb.time = &when;
  cb.cb_rtn = next_cb;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot re-register AfterDelay call back\n");
  return 0;
}

void my_handle_register()
{
  when.type = vpiSimTime;
  when.low = 1000;
  when.high = 0;
  cb.reason = cbAfterDelay;
  cb.time = &when;
  cb.cb_rtn = &start_cb;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot register AfterDelay call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
