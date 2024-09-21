#include <stdio.h>
#include <vpi_user.h>
#include <stdlib.h>
#include <string.h>

static const struct {
  const char *name;
  PLI_INT32   direction, size;
} expected[] = {{ "in1", vpiInput, 2},
                { "out1", vpiOutput, 3},
                { "inout1", vpiInout, 4},
                { "inout2", vpiNoDirection, 3}, // !
                { NULL, }
};

PLI_INT32
vpi_proc (s_cb_data *cb)
{
  vpiHandle      iter, top, item;
  PLI_INT32      direction, size;
  char          *name;
  int            i;

  /* Find the (unique?) top-level module and the one inside it. */

  iter = vpi_iterate(vpiModule, NULL);
  top = vpi_scan(iter);
  vpi_free_object(iter);
  vpi_printf("Top %s\n", vpi_get_str(vpiName, top));

  /* Get the ports. */

  iter = vpi_iterate(vpiPort, top);
  i = 0;
  while ((item = vpi_scan(iter))) {
    direction = vpi_get(vpiDirection, item);
    name = vpi_get_str(vpiName, item);
    size = vpi_get(vpiSize, item);

    vpi_printf("Port %s direction %d size %d, type %d\n",
               name, direction, size, vpi_get(vpiType, item));

    if (!expected[i].name) {
        vpi_printf("Error: found extra port %s.\n", name);
        exit(1);
    }

    if (strcmp(name, expected[i].name)) {
      vpi_printf("Error: found port %s but expected %s.\n",
                 name, expected[i].name);
    }
    if (direction != expected[i].direction) {
      vpi_printf("Error: found port %s direction %d but expected %d.\n",
                 name, direction, expected[i].direction);
    }
    if (size != expected[i].size) {
      vpi_printf("Error: found port %s size %d but expected %d.\n",
                 name, size, expected[i].size);
    }
    ++i;
  }

  if (expected[i].name)
    vpi_printf("Error: missing port %s.\n", expected[i].name);
  return 0;
}

void my_handle_register()
{
  s_cb_data cb;
  printf ("Hello world\n");

  cb.reason = cbEndOfCompile;
  cb.cb_rtn = &vpi_proc;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot register EndOfCompile call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
