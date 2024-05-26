#include <stdio.h>
#include <vpi_user.h>
#define N_NAMES 0

PLI_INT32
vpi_proc (s_cb_data *cb)
{
  vpiHandle net;
  s_vpi_value val;
  char names[N_NAMES][64] =
    {
     // Array 2 dimensional (Not supported)
     //"myentity.sigarray2dim",
     //"myentity.portarray2dim",
     //"myentity.genarray2dim",
     //"myentity.constarray2dim"

     // Array of bit_vectors (Not supported)
     //"myentity.sigarray1",
     //"myentity.portarray1",
     //"myentity.genarray1",
     //"myentity.constarray1"
    };
  int name_index;
                       
  for (name_index=0; name_index<N_NAMES; name_index+=1) {
    printf ("Trying to find name %s\n", names[name_index]);
    net = vpi_handle_by_name (names[name_index], NULL);
    if (net == NULL)
        {
        printf ("Error: Failed to find the net %s\n", names[name_index]);
        return -1;
        }
    val.format = vpiBinStrVal;
    vpi_get_value (net, &val);
    printf ("value: %s\n", val.value.str);
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
