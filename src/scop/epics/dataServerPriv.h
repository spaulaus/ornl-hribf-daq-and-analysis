#ifndef __dataServerPriv_h
#define __dataServerPriv_h 1

#include "dataServer.h"

#define ERR_NOMEM     2
#define ERR_BADSTATE  4
#define ERR_BADHANDLE 6
#define ERR_TIMEOUT   8
#define ERR_WRITE     10
#define ERR_REGEXP    12
#define ERR_BADNAME   14
#define ERR_UNIX      20

#define STATE_NOPVS        100
#define STATE_PVSALLOCATED 101

#define SETPVNAMES 'S'
#define FREEPVS    'F'
#define WRITEALL   'A'
#define WRITEONE   'O'
#define DISCONNECT 'D'
#define KILLSERVER 'K'

#define MAXPVS 100
#define MAXLEN 2000

typedef struct privDsHandleTag {
  int fd;
  int connectionEstablished;
  int pvsAllocated;
  int unixErr;
  int ipAddr;
  char errString[127+1];
  unsigned short portNum;
} privDsHandleType, *privDsHandlePtr;

#endif
