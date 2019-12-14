#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

extern int ghdl_main (int argc, char **argv);

uint8_t *D[1];
uintptr_t get_addr(uint8_t id) { return (uintptr_t)D[id]; }
uintptr_t get_baddr(uint8_t id) { return get_addr(id); }

int main(int argc, char **argv) {
  const uint32_t length = 3;
  D[0] = (uint8_t *) malloc(2*length*sizeof(uint8_t));
  if ( D[0] == NULL ) {
    perror("execution of malloc() failed!\n");
    return -1;
  }
  int i;
  for(i=0; i<length; i++) { D[0][i] = (i+1)*10; }
  for(i=0; i<2*length; i++) { printf("%d: %d\n", i, D[0][i]); }
  ghdl_main(argc, argv);
  for(i=0; i<2*length; i++) { printf("%d: %d\n", i, D[0][i]); }
  free(D[0]);
  return 0;
}
