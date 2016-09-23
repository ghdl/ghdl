#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>

int sim_init_queue (int qid)
{
  key_t  key;
  int    msgqueue_id;
  //struct mymsgbuf qbuf;

  /* Create unique key via call to ftok() */
  key = ftok(".", 'm');

  /* Open the queue - create if necessary */
  if((msgqueue_id = msgget(key, IPC_CREAT|0660)) == -1) {
          perror("msgget");
          exit(1);
  }
  return msgqueue_id;

}

int sim_delete_queue(int qid)
{

  /* Remove the queue */
  msgctl(qid, IPC_RMID, 0);

  return 0;

}

