#include <stdio.h>
#include <vhpi_user.h>


void my_callback(const vhpiCbDataT * cb_data) {
}

void my_startup()
{
  printf ("VHPI lib\n");

  vhpi_printf("VHPI printf\n");

  vhpiCbDataT cb_data;
  vhpiTimeT time;

  cb_data.reason = vhpiCbStartOfSimulation;
  cb_data.cb_rtn = my_callback;
  cb_data.obj = NULL;
  cb_data.time = &time;
  cb_data.value = NULL;
  cb_data.user_data = (char *)NULL;
  time.high = 0;
  time.low = 0;

  vhpi_register_cb(&cb_data, NULL);
}

void (*vhpi_startup_routines[]) () =
{
  my_startup,
  0
};
