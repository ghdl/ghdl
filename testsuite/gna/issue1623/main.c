#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

extern int ghdl_main(int argc, void** argv);

void sigabrtHandler(int sig_num) {
  // Reset handler to catch SIGABRT next time. Refer http://en.cppreference.com/w/c/program/signal
  signal(SIGABRT, sigabrtHandler);
  printf("SIGABRT caught %d!\n", sig_num);
  fflush(stdout);
}

static void exit_handler(void) {
  printf("This is the exit handler.\n");
}

int entry(int argc, void** argv) {
  signal(SIGABRT, sigabrtHandler);
  atexit(exit_handler);

  printf("Hello entry!\n");
  int ecode = ghdl_main(argc, argv);
  printf("Bye entry <%d>!\n", ecode);

  return ecode;
}
