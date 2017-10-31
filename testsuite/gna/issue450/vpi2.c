/*****************************************************************************

 * Test vpi program
 * vpi handle  File = printNets.c

*****************************************************************************/

#include <stdio.h>
#include <vpi_user.h>

/*****************************************************************************
 * User program
 * my handle
 *****************************************************************************/

void printContent (vpiHandle parent)
{
  vpiHandle Iterator;

  vpi_printf ("Full module name (vpiFullName): \t%s\n", vpi_get_str (vpiFullName, parent));
  vpi_printf ("Simple module name (vpiName): \t\t%s\n", vpi_get_str (vpiName, parent));

  Iterator = vpi_iterate (vpiNet, parent);
  if (Iterator)
    {
      vpiHandle netHandle;
      while ((netHandle = vpi_scan (Iterator)))
	{
	  char *NetName = vpi_get_str (vpiName, netHandle);
	  vpi_printf (" net %s\n", NetName);
	}
    }

  Iterator = vpi_iterate (vpiModule, parent);
  if (Iterator)
    {
      vpiHandle scopeHandle;
      while ((scopeHandle = vpi_scan (Iterator)))
	printContent (scopeHandle);
    }
}

void printModules()
{
  vpiHandle topModIterator;
  vpiHandle topModHandle;
  
  char *ModName;

  vpi_printf ("got to here \n");
  /* create a module iterator that starts at the top  as indicated by NULL */
  topModIterator = vpi_iterate (vpiModule, NULL);
  vpi_printf ("got to here1 \n");
  if (!topModIterator)
    {
      return;
    }

  /* use vpi_scan to iterate throught modules */
  while ((topModHandle = vpi_scan (topModIterator)))
    {
      ModName = vpi_get_str (vpiName, topModHandle);
      vpi_printf ("Module %s:\n", ModName);

      printContent (topModHandle);
    }
}


/*****************************************************************************
 * Creating  structure
 *****************************************************************************/

void my_handle_register()
{
#if 0
  s_vpi_systf_data tf_data;

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$printNets";
  tf_data.calltf    = printModules;
  tf_data.compiletf = 0;
  tf_data.sizetf    = 0;
  vpi_register_systf(&tf_data);
#else
  s_cb_data cb;

  cb.reason = cbEndOfCompile;
  cb.cb_rtn = &printModules;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("cannot register EndOfCompile call back\n");
#endif
}
// register the  task

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};

/*  Makefile:
#############################

##Sample make file to compile a vpi routine with iverilog
CC = gcc
OBJECTS = printNets.o
DLL = printNets.vpi
CFLAG = -0

#compile all the objects
.c.o:;
 $(CC) -c -g -o   $@ $<

all: $(DLL) graycntr.v
 iverilog -N graycntr.v -o graycntr -m ./printNets.vpi

$(DLL): $(OBJECTS)
 $(CC) -o $(DLL) -shared $(OBJECTS) -lvpi

clean :
 rm -rf *.o
 rm -rf *.vpi
 rm -rf *~
 rm -rf core
*/


