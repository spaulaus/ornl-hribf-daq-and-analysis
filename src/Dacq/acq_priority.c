#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>

extern int errno;
int main(int argc, char *argv[])
{
  int pid,new_prio,old_prio,status;

  if (argc < 3) 
    {
      printf("Usage:  acq_priority PID priority\n");
      exit(EXIT_FAILURE);
    }
  if (sscanf(argv[1],"%i",&pid) != 1)
    {
      printf("Illegal PID value\n");
      exit(EXIT_FAILURE);
    }
  if (sscanf(argv[2],"%i",&new_prio) != 1)
    {
      printf("Invalid priority request\n");
      exit(EXIT_FAILURE);
    }
  if (new_prio < -20 || new_prio > 20)
    {
      printf("Invalid priority request\n");
      exit(EXIT_FAILURE);
    }
  errno = 0;
  old_prio = getpriority(PRIO_PROCESS,pid);
  if (old_prio == -1 && errno != 0)
    {
      perror("getpriority");
      exit(EXIT_FAILURE);
    }
  status = setpriority(PRIO_PROCESS,pid,new_prio);
  if (status == -1)
    {
      perror("setpriority");
      exit(EXIT_FAILURE);
    } 
  printf("PID = %i, Old Priority = %i, New Priority = %i\n",
                                                       pid,old_prio,new_prio);
}
