/* Test for record (struct) signals and ports: vpiStructNet, vpiMember.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vpi_user.h>

static s_vpi_time when;
static s_cb_data  cb;
static int        nerrors;
static int        nchecks;

#define CHECK(COND, ...)                                                \
  do {                                                                  \
    ++nchecks;                                                          \
    if (!(COND)) {                                                      \
      vpi_printf ("Error: %s:%d: ", __FILE__, __LINE__);                \
      vpi_printf (__VA_ARGS__);                                         \
      vpi_printf ("\n");                                                \
      ++nerrors;                                                        \
    }                                                                   \
  } while (0)

/* Find the object named NAME in the objects of type TYPE of REF.  */
static vpiHandle
find (int type, vpiHandle ref, const char *name)
{
  vpiHandle it = vpi_iterate (type, ref);
  vpiHandle h;

  if (it == NULL)
    return NULL;
  while ((h = vpi_scan (it)) != NULL) {
    const char *n = vpi_get_str (vpiName, h);
    if (n != NULL && strcmp (n, name) == 0) {
      vpi_free_object (it);
      return h;
    }
    vpi_free_object (h);
  }
  return NULL;
}

/* Check the members of the record REC, whose full name is PATH.  The
   members are NAMES (NULL terminated), of type TYPES and size SIZES.  */
static void
check_members (vpiHandle rec, const char *path, const char *const names[],
               const int types[], const int sizes[])
{
  vpiHandle it = vpi_iterate (vpiMember, rec);
  vpiHandle m;
  int i = 0;

  CHECK (it != NULL, "no members for %s", path);
  if (it == NULL)
    return;
  while ((m = vpi_scan (it)) != NULL) {
    char full[128];

    CHECK (names[i] != NULL, "too many members in %s", path);
    if (names[i] == NULL)
      break;
    CHECK (strcmp (vpi_get_str (vpiName, m), names[i]) == 0,
           "member %d of %s is %s", i, path, vpi_get_str (vpiName, m));
    snprintf (full, sizeof full, "%s.%s", path, names[i]);
    CHECK (strcmp (vpi_get_str (vpiFullName, m), full) == 0,
           "full name %s instead of %s", vpi_get_str (vpiFullName, m), full);
    CHECK (vpi_get (vpiType, m) == types[i],
           "type of %s is %d", full, vpi_get (vpiType, m));
    if (types[i] == vpiNet)
      CHECK (vpi_get (vpiSize, m) == sizes[i],
             "size of %s is %d", full, vpi_get (vpiSize, m));
    vpi_free_object (m);
    ++i;
  }
  CHECK (names[i] == NULL, "missing member %s in %s", names[i], path);
}

static void
check_int (vpiHandle h, const char *name, int expected)
{
  s_vpi_value val;

  val.format = vpiIntVal;
  vpi_get_value (h, &val);
  CHECK (val.value.integer == expected, "%s is %d, %d expected", name,
         val.value.integer, expected);
}

static void
check_bin (vpiHandle h, const char *name, const char *expected)
{
  s_vpi_value val;

  val.format = vpiBinStrVal;
  vpi_get_value (h, &val);
  CHECK (strcmp (val.value.str, expected) == 0, "%s is %s, %s expected",
         name, val.value.str, expected);
}

static void
check_scalar (vpiHandle h, const char *name, int expected)
{
  s_vpi_value val;

  val.format = vpiScalarVal;
  vpi_get_value (h, &val);
  CHECK (val.value.scalar == expected, "%s is %d, %d expected", name,
         val.value.scalar, expected);
}

static vpiHandle
member (vpiHandle rec, const char *name)
{
  vpiHandle h = find (vpiMember, rec, name);
  CHECK (h != NULL, "no member %s", name);
  return h;
}

static vpiHandle rs, rp, ro, rc, rs_int, rs_vec, rs_cnt, rp_bit;
static int cnt_int, cnt_flag, cnt_vec;

static PLI_INT32
int_cb (s_cb_data *pcb)
{
  ++cnt_int;
  CHECK (pcb->value->value.integer == 5, "rc.int_f callback value is %d",
         pcb->value->value.integer);
  return 0;
}

static PLI_INT32
flag_cb (s_cb_data *pcb)
{
  ++cnt_flag;
  CHECK (pcb->value->value.scalar == vpi1, "rc.inner.flag callback value");
  return 0;
}

static PLI_INT32
vec_cb (s_cb_data *pcb)
{
  ++cnt_vec;
  CHECK (strcmp (pcb->value->value.str, "1111") == 0,
         "rc.vec_f callback value is %s", pcb->value->value.str);
  return 0;
}

static void
register_cb (vpiHandle h, int format, PLI_INT32 (*rtn) (s_cb_data *))
{
  static s_vpi_value val[3];
  static int n;

  val[n].format = format;
  cb.reason = cbValueChange;
  cb.cb_rtn = rtn;
  cb.obj = h;
  cb.value = &val[n++];
  CHECK (vpi_register_cb (&cb) != NULL, "cannot register callback");
}

/* Step 1: discover the objects and read the initial values.  */
static PLI_INT32
step1_cb (s_cb_data *pcb)
{
  static const char *const rec_names[] =
    { "bit_f", "vec_f", "int_f", "inner", NULL };
  static const int rec_types[] = { vpiNet, vpiNet, vpiNet, vpiStructNet };
  static const int rec_sizes[] = { 1, 4, 32, 0 };
  static const char *const inner_names[] = { "flag", "cnt", NULL };
  static const int inner_types[] = { vpiNet, vpiNet };
  static const int inner_sizes[] = { 1, 32 };
  vpiHandle it, mod, h, inner;
  int nrec = 0;

  it = vpi_iterate (vpiModule, NULL);
  mod = vpi_scan (it);
  CHECK (mod != NULL, "no top module");
  vpi_free_object (it);

  /* Records are returned when iterating on nets, arrays of records are
     skipped.  */
  it = vpi_iterate (vpiNet, mod);
  while ((h = vpi_scan (it)) != NULL) {
    const char *n = vpi_get_str (vpiName, h);
    if (vpi_get (vpiType, h) == vpiStructNet)
      ++nrec;
    CHECK (strcmp (n, "ra") != 0, "array of records found");
  }
  CHECK (nrec == 4, "%d records found", nrec);

  rp = find (vpiNet, mod, "rp");
  ro = find (vpiNet, mod, "ro");
  rs = find (vpiNet, mod, "rs");
  rc = find (vpiNet, mod, "rc");
  CHECK (rp && ro && rs && rc, "records not found");
  if (!(rp && ro && rs && rc))
    return 0;
  CHECK (vpi_get (vpiType, rs) == vpiStructNet, "rs is not a struct");
  CHECK (strcmp (vpi_get_str (vpiFullName, rs), "myentity.rs") == 0,
         "full name of rs is %s", vpi_get_str (vpiFullName, rs));

  check_members (rs, "myentity.rs", rec_names, rec_types, rec_sizes);
  check_members (rp, "myentity.rp", rec_names, rec_types, rec_sizes);
  inner = member (rs, "inner");
  check_members (inner, "myentity.rs.inner", inner_names, inner_types,
                 inner_sizes);

  /* Initial values.  */
  check_scalar (member (rs, "bit_f"), "rs.bit_f", vpi0);
  check_bin (member (rs, "vec_f"), "rs.vec_f", "0000");
  check_int (member (rs, "int_f"), "rs.int_f", 1);
  check_scalar (member (inner, "flag"), "rs.inner.flag", vpi1);
  check_int (member (inner, "cnt"), "rs.inner.cnt", 2);

  check_scalar (member (rp, "bit_f"), "rp.bit_f", vpi1);
  check_bin (member (rp, "vec_f"), "rp.vec_f", "1010");
  check_int (member (rp, "int_f"), "rp.int_f", 42);
  h = member (rp, "inner");
  check_int (member (h, "cnt"), "rp.inner.cnt", -7);

  check_scalar (member (ro, "bit_f"), "ro.bit_f", vpi1);
  check_bin (member (ro, "vec_f"), "ro.vec_f", "0011");
  check_int (member (ro, "int_f"), "ro.int_f", 5);
  check_scalar (member (member (ro, "inner"), "flag"), "ro.inner.flag",
                vpiZ);

  /* By name.  */
  h = vpi_handle_by_name ("myentity.rs.inner.cnt", NULL);
  CHECK (h != NULL, "rs.inner.cnt not found by name");
  if (h != NULL) {
    check_int (h, "rs.inner.cnt (by name)", 2);
    CHECK (strcmp (vpi_get_str (vpiFullName, h), "myentity.rs.inner.cnt") == 0,
           "full name is %s", vpi_get_str (vpiFullName, h));
  }
  CHECK (vpi_handle_by_name ("myentity.rs.nosuch", NULL) == NULL,
         "unknown member found");
  h = vpi_handle_by_name ("myentity.rs", NULL);
  CHECK (h != NULL && vpi_get (vpiType, h) == vpiStructNet,
         "rs not found by name");

  /* Arrays of records are not handled, but must not crash.  */
  h = vpi_handle_by_name ("myentity.ra", NULL);
  CHECK (h == NULL || vpi_handle_by_index (h, 0) == NULL,
         "array of records is handled");

  /* A record has no value.  */
  CHECK (vpi_iterate (vpiMember, member (rs, "int_f")) == NULL,
         "members of a scalar");

  /* No value change callback on a record, and records are not packed.  */
  cb.reason = cbValueChange;
  cb.cb_rtn = int_cb;
  cb.obj = rs;
  cb.value = NULL;
  CHECK (vpi_register_cb (&cb) == NULL, "callback on a record");
  CHECK (vpi_get (vpiPacked, rs) == 0, "packed record");

  /* The members of a port have the direction of the port.  */
  CHECK (vpi_get (vpiDirection, member (rp, "int_f")) == vpiInout,
         "direction of rp.int_f");
  CHECK (vpi_get (vpiDirection, member (member (ro, "inner"), "cnt"))
         == vpiOutput, "direction of ro.inner.cnt");

  /* Value change callbacks on members.  */
  register_cb (member (rc, "int_f"), vpiIntVal, int_cb);
  register_cb (member (member (rc, "inner"), "flag"), vpiScalarVal, flag_cb);
  register_cb (member (rc, "vec_f"), vpiBinStrVal, vec_cb);

  rs_int = member (rs, "int_f");
  rs_vec = member (rs, "vec_f");
  rs_cnt = member (inner, "cnt");
  rp_bit = member (rp, "bit_f");
  return 0;
}

/* Step 2: write members.  The VHDL side checks the new values.  */
static PLI_INT32
step2_cb (s_cb_data *pcb)
{
  s_vpi_value val;

  val.format = vpiIntVal;
  val.value.integer = 99;
  vpi_put_value (rs_int, &val, NULL, vpiNoDelay);
  val.value.integer = -3;
  vpi_put_value (rs_cnt, &val, NULL, vpiNoDelay);

  val.format = vpiBinStrVal;
  val.value.str = "0101";
  vpi_put_value (rs_vec, &val, NULL, vpiNoDelay);

  val.format = vpiScalarVal;
  val.value.scalar = vpi0;
  vpi_put_value (rp_bit, &val, NULL, vpiNoDelay);
  return 0;
}

/* Step 3: read back what was written.  */
static PLI_INT32
step3_cb (s_cb_data *pcb)
{
  check_int (rs_int, "rs.int_f", 99);
  check_int (rs_cnt, "rs.inner.cnt", -3);
  check_bin (rs_vec, "rs.vec_f", "0101");
  check_scalar (rp_bit, "rp.bit_f", vpi0);
  return 0;
}

static PLI_INT32
end_cb (s_cb_data *pcb)
{
  CHECK (cnt_int == 1, "%d callbacks for rc.int_f", cnt_int);
  CHECK (cnt_flag == 1, "%d callbacks for rc.inner.flag", cnt_flag);
  CHECK (cnt_vec == 1, "%d callbacks for rc.vec_f", cnt_vec);
  if (nerrors == 0)
    vpi_printf ("All %d record checks passed.\n", nchecks);
  return 0;
}

static void
at (int ns, PLI_INT32 (*rtn) (s_cb_data *))
{
  when.type = vpiSimTime;
  when.low = ns * 1000;
  when.high = 0;
  cb.reason = cbAfterDelay;
  cb.time = &when;
  cb.cb_rtn = rtn;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot register AfterDelay call back\n");
}

void my_handle_register()
{
  at (1, step1_cb);
  at (3, step2_cb);
  at (4, step3_cb);

  cb.reason = cbEndOfSimulation;
  cb.cb_rtn = end_cb;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot register EndOfSimulation call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
