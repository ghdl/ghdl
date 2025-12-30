#include <stdio.h>
#include <vpi_user.h>
#include <stdlib.h>
#include <string.h>

PLI_INT32
vpi_proc (s_cb_data *cb)
{
  vpiHandle      iter, top, item;
  PLI_INT32      direction, size, sgn;
  char          *name;

  /* Find the (unique?) top-level module and the one inside it. */

  iter = vpi_iterate(vpiModule, NULL);
  top = vpi_scan(iter);
  vpi_free_object(iter);
  vpi_printf("Top %s\n", vpi_get_str(vpiName, top));

  /* Get the ports. */

  iter = vpi_iterate(vpiPort, top);
  while ((item = vpi_scan(iter))) {
    direction = vpi_get(vpiDirection, item);
    name = vpi_get_str(vpiName, item);
    size = vpi_get(vpiSize, item);
    sgn = vpi_get(vpiSigned, item);

    vpi_printf("Port %s direction %d size %d, sgn %d, type %d\n",
               name, direction, size, sgn, vpi_get(vpiType, item));
  }
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
