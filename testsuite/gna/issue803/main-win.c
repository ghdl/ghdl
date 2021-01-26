#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {

  void* h = LoadLibraryA(".\\tb.dll");
  if (!h){
    fprintf(stderr, "error: cannot load library\n");
    exit(1);
  }

  typedef int main_t(int, char**);

  main_t* ghdl_main = (main_t*)GetProcAddress(h, "_ghdl_main");
  if (!ghdl_main){
    fprintf(stderr, "error: cannot find symbol\n");
    exit(2);
  }

  printf("ghdl_main return: %d\n", ghdl_main(argc, argv));

  FreeLibrary(h);

  return 0;

}
