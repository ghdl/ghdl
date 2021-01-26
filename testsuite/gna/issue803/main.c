#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {

  void* h = dlopen("./tb.so", RTLD_LAZY);
  if (!h){
    fprintf(stderr, "%s\n", dlerror());
    exit(1);
  }

  typedef int main_t(int, char**);

  h = dlopen("./tb.so", RTLD_LAZY);
  if (!h){
    fprintf(stderr, "%s\n", dlerror());
    exit(1);
  }

  main_t* ghdl_main = (main_t*)dlsym(h, "ghdl_main");
  if (!ghdl_main){
    fprintf(stderr, "%s\n", dlerror());
    exit(2);
  }

  printf("ghdl_main return: %d\n", ghdl_main(argc, argv));

  dlclose(h);

  return 0;

}
