#include <stdio.h>
#include <vpi_user.h>
#define N_NAMES 12

PLI_INT32
vpi_proc (s_cb_data *cb)
{
  s_vpi_vlog_info info;
  int i;
  printf ("Trying to get vlog_info\n");
  int ret = vpi_get_vlog_info(&info);
  if (ret != 1) {
    printf ("Error: Failed to get vlog_info\n");
    return -1;
  }

  if (info.argc < 1) {
     printf ("Error: Argc was 0\n");
     return -1;
  }
  printf ("Argc: %d\n", info.argc);

  for (i = 0; i < info.argc; i++) {
   printf ("Argv[%d]: %s\n", i, info.argv[i]);
  }

  if (info.product == NULL) {
    printf ("Error: product is NULL\n");
    return -1;
  }
  printf ("Product: %s\n", info.product);

  if (info.version == NULL) {
    printf ("Error: version is NULL\n");
    return -1;
  }
  printf ("Version: %s\n", info.version);
  return 0;
}

void my_handle_register()
{
  s_cb_data cb;
  printf ("Hello world\n");

  cb.reason = cbStartOfSimulation;
  cb.cb_rtn = &vpi_proc;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot register StartOfSimulation call back\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
