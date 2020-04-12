#include <stdio.h>

extern int ghdl_main(int argc, char** argv);

int main(int argc, char** argv) {
  printf("ghdl_main: %d\n", ghdl_main(argc, argv));
  return 0;
}
