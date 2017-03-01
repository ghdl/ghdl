/*
  VPI code allowing you to connect terminal emulator or other program to pty "connected"
  to the UART-like port in IP core simulated in GHDL.
 
  This code is written by Wojciech M. Zabolotny (wz...@ise.pw.edu.pl) on 2nd June 2011
  and is published as PUBLIC DOMAIN
 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <linux/ioctl.h>
#include <sys/stat.h>
#include <sys/poll.h>
#include <fcntl.h>
#include <errno.h>

char *ptsname(int fd);

int ptyf = -1;



int tty_open(int a)
{
  ptyf = open("/dev/ptmx",O_RDWR);
  if(ptyf<0) {
    perror("I can't open pseudoterminal\n");   
    return -1;
  }
  if(unlockpt(ptyf)<0) {
    perror("I can't unlock pseudoterminal\n");
    return -1;
  }
  if(grantpt(ptyf)<0) {
    perror("I can't grant pseudoterminal\n");
    return -1;
  }
  printf("Pseudoterminal: %s\n",ptsname(ptyf));
 // sleep(10);
  return 0;
}


int read_enable(void)
{
  //In the masks below you may omit POLLHUP in this case
  //disconnection of the terminal emulator from pty will not
  //stop simulation, and you'll be able to reconnect
  //the same or different program to pty and running simulation
  struct pollfd pfd[1]={{ptyf,POLLIN | POLLERR | POLLHUP,0}};
  int res;
  res=poll(pfd,1,0);
  if(res==0) return 0;
  if(res<0) return 0; //error
  //If you removed POLLHUP from the mask above, you should remove it below too
  if(pfd[0].revents & (POLLERR|POLLHUP)) return 0; //disconnected or error?	
  if(pfd[0].revents & POLLIN)       return 1;
  return 0;
}


int read_data(void)
{
  unsigned char c;
  read(ptyf,&c,1);
  return c;
}

int write_data(int byte)
{
  unsigned char c = byte;
  write(ptyf,&c,1);  

  // Debug: printf("Writing %x to pty\n", c);
  return 0;
}

