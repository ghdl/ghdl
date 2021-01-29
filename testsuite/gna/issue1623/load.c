#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {

  void* h = dlopen(argv[1], RTLD_LAZY);
  if (!h){
    fprintf(stderr, "dlopen: %s\n", dlerror());
    exit(1);
  }

  typedef int main_t(int, char**);

  main_t* entry = (main_t*)dlsym(h, "entry");
  if (!entry){
    fprintf(stderr, "dlsym: %s\n", dlerror());
    exit(2);
  }

  printf("Call entry\n");
  printf("Return from entry: %d\n", entry(0, NULL));

  dlclose(h);

  return 0;

}
