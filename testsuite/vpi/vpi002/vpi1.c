#include <stdio.h>
#include <vpi_user.h>
#define N_NAMES 12

void
vpi_proc (void)
{
  vpiHandle net;
  s_vpi_value val;
  char names[N_NAMES][64] =
    {
     // Enum
     "myentity.sigenum",
     "myentity.portenum",
     "myentity.genenum",
     //"myentity.constenum" // Not supported

     // Array1 (unbounded static)
     "myentity.sigarray1",
     "myentity.portarray1",
     //"myentity.genarray1",  // Not supported
     //"myentity.constarray1" // Not supported
     //"myentity.sigarray1[0]", // Not supported
     //"myentity.portarray1[0]", // Not supported

     // Array2 (unbounded complex)
     "myentity.sigarray2",
     "myentity.portarray2",
     //"myentity.constarray2" // Not supported

     // Array3 (bounded static)
     "myentity.sigarray3",
     "myentity.portarray3",
     //"myentity.genarray3",  // Not supported
     //"myentity.constarray3" // Not supported

     // Array4 (bounded complex)
     "myentity.sigarray4",
     //"myentity.constarray4" // Not supported

     // Array4 (bounded static) array of bit
     "myentity.sigarray5",
     "myentity.portarray5",
     //"myentity.constarray5", // Not supported
     //"myentity.genarray5", // Not supported
     //"myentity.sigarray5[0]", // Not supported
     //"myentity.portarray5[0]" // Not supported
    };
  int name_index;
                       
  for (name_index=0; name_index<N_NAMES; name_index+=1) {
    printf ("Trying to find name %s\n", names[name_index]);
    net = vpi_handle_by_name (names[name_index], NULL);
    if (net == NULL)
        {
        printf ("Error: Failed to find the net %s\n", names[name_index]);
        return;
        }
    val.format = vpiBinStrVal;
    vpi_get_value (net, &val);
    printf ("value: %s\n", val.value.str);
  }
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
