#include <unistd.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include "ctype.h"
#include "math.h"
#include <signal.h>
#include <setjmp.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <math.h>
#include <regex.h>

#include "dataServer.version"
#include "dataServerPriv.h"

static const char *version = VERSION;

static const char *errArray[] = {
  "Unknown error code",
  "Success",
  "Insufficient memory",
  "Error 3",
  "Operation not permitted",
  "Error 5",
  "Invalid object handle",
  "Error 7",
  "Network communication timeout",
  "Error 9",
  "Network write operation failed",
  "Error 11",
  "Reg expr error",
  "Error 13",
  "Invalid PV name",
  "Error 15",
  "Error 16",
  "Error 17",
  "Error 18",
  "Error 19",
  "Unix error"
};

static int sendMsg (
  dsHandle connection,
  char *msg
) {

privDsHandlePtr priv;
struct timeval timeout;
int more, i, remain, len, fd;
fd_set fds;

  //printf( "sendMsg, msg = [%s]\n", msg );

  priv = (privDsHandlePtr) connection;
  if ( !priv ) {
    return ERR_BADHANDLE;
  }

  timeout.tv_sec = 10;
  timeout.tv_usec = 0;

  more = 1;
  i = 0;
  remain = strlen(msg);
  while ( more ) {

    FD_ZERO( &fds );
    FD_SET( priv->fd, &fds );

    fd = select( FD_SETSIZE, (fd_set *) NULL, &fds,
     (fd_set *) NULL, &timeout );

    if ( fd == 0 ) { /* timeout */
      return ERR_TIMEOUT;
    }

    if ( fd < 0 ) { /* error */
      priv->unixErr = errno;
      return ERR_UNIX;
    }

    len = write( priv->fd, &msg[i], remain );
    if ( len < 1 ) {
      if ( errno ) {
        priv->unixErr = errno;
        return ERR_UNIX;
      }
      return ERR_WRITE;
    }

    remain -= len;
    i += len;

    if ( remain < 1 ) more = 0;

  } while ( more );

  return ERR_SUCCESS;

}

char *dsGetErrorText (
  dsHandle connection,
  int errCode
) {

privDsHandlePtr priv;

  priv = (privDsHandlePtr) connection;
  if ( !priv ) return (char *) errArray[6]; // Invalid object handle

  if ( errCode > ERR_UNIX ) errCode = 0;
  if ( errCode < 1 ) errCode = 0;

  if ( errCode == ERR_UNIX ) {
    strncpy( priv->errString, strerror( priv->unixErr ), 127 );
    //strerror_r( priv->unixErr, priv->errString, 127 ); // doesn't work
  }
  else {
    strncpy( priv->errString, errArray[errCode], 127 );
  }

  return priv->errString;

}

void dsShowError (
  dsHandle connection,
  int errCode
) {

privDsHandlePtr priv;

  priv = (privDsHandlePtr) connection;
  if ( !priv ) {
    fprintf( stderr, "dslib: %s\n", errArray[6] ); // Invalid object handle
    return;
  }

  if ( errCode > ERR_UNIX ) errCode = 0;
  if ( errCode < 1 ) errCode = 0;

  if ( errCode == ERR_UNIX ) {
    strncpy( priv->errString, strerror( priv->unixErr ), 127 );
    //strerror_r( priv->unixErr, priv->errString, 127 ); // doesn't work
  }
  else {
    strncpy( priv->errString, errArray[errCode], 127 );
  }

  fprintf( stderr, "dslib: %s\n", priv->errString );

}

int dsInit (
  char *addr,
  int port,
  dsHandle *connection
) {

privDsHandlePtr priv;
unsigned int value, len;
struct sockaddr_in sin;
int stat;

  priv = calloc( 1, sizeof(privDsHandleType) );
  if ( !priv ) {
    *connection = NULL;
    return ERR_NOMEM;
  }

  *connection = (dsHandle) priv;

  priv->fd = -1;
  priv->connectionEstablished = 0;
  priv->pvsAllocated = 0;
  priv->unixErr = 0;
  priv->ipAddr = inet_addr( addr );
  priv->portNum = htons( (unsigned short) port );

  priv->fd = socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );
  if ( priv->fd == -1 ) {
    priv->unixErr = errno;
    return ERR_UNIX;
  }

  value = 1;
  len = sizeof(value);
  stat = setsockopt( priv->fd, IPPROTO_TCP, TCP_NODELAY,
   (char *) &value, len );
  if ( stat < 0 ) {
    priv->unixErr = errno;
    return ERR_UNIX;
  }

  value = 1;
  len = sizeof(value);
  stat = setsockopt( priv->fd, SOL_SOCKET, SO_KEEPALIVE,
   (char *) &value, len );
  if ( stat < 0 ) {
    priv->unixErr = errno;
    return ERR_UNIX;
  }

  bzero( (char *) &sin, sizeof(sin) );
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = priv->ipAddr;
  sin.sin_port = priv->portNum;

  stat = connect( priv->fd, (struct sockaddr *) &sin,
   sizeof(sin) );
  if ( stat < 0 ) {
    priv->unixErr = errno;
    close( priv->fd );
    priv->fd = -1;
    return ERR_UNIX;
  }

  priv->connectionEstablished = 1;

  return ERR_SUCCESS;

}

int dsTerminate (
  dsHandle connection
) {

int stat;
privDsHandlePtr priv;

  priv = (privDsHandlePtr) connection;
  if ( !priv ) {
    return ERR_BADHANDLE;
  }

  if ( priv->connectionEstablished ) {

    priv->connectionEstablished = 0;

    // send disconnect command
    stat = sendMsg( priv, "D\n" );

    // disconnect asychronously
    stat = shutdown( priv->fd, 2 );
    stat = close( priv->fd );

  }

  free( priv );
  priv = (privDsHandlePtr) NULL;

  return ERR_SUCCESS;

}

int dsAllocPvs (
  dsHandle connection,
  char *pvList
) {

privDsHandlePtr priv;
char *ctx, *tk;
int stat, invalidName;
regex_t reg;
regmatch_t match[10];
char buf[2000+1];

  priv = (privDsHandlePtr) connection;
  if ( !priv ) {
    return ERR_BADHANDLE;
  }

  stat = regcomp( &reg, "scal:(bl21|bl23|drs|rms|enge):cnt[0-9]+",
   REG_EXTENDED );
  if ( stat ) {
    perror( "regcomp" );
    return ERR_REGEXP;
  }

  strncpy( buf, pvList, 2000 );
  buf[2000] = 0;

  ctx = NULL;
  tk = strtok_r( buf, ", \t\n", &ctx );
  while ( tk ) {
    invalidName = regexec( &reg, tk, 1, match, 0 );
    if ( invalidName ) return ERR_BADNAME;
    tk = strtok_r( NULL, ", \t\n", &ctx );
  }

  regfree( &reg );

  stat = sendMsg( priv, "S" );
  if ( !( stat & 1 ) ) return stat;

  stat = sendMsg( priv, pvList );
  if ( !( stat & 1 ) ) return stat;

  stat = sendMsg( priv, "\n" );
  if ( !( stat & 1 ) ) return stat;

  return ERR_SUCCESS;

}


int dsWriteAll (
  dsHandle connection,
  char *valueList
) {

privDsHandlePtr priv;
int stat;
char buf[1023+1];

  priv = (privDsHandlePtr) connection;
  if ( !priv ) {
    return ERR_BADHANDLE;
  }

  if ( strlen(valueList) < 1021 ) {

    snprintf( buf, 1023, "A%s\n", valueList );
    stat = sendMsg( priv, buf );
    if ( !( stat & 1 ) ) return stat;

  }
  else {

    stat = sendMsg( priv, "A" );
    if ( !( stat & 1 ) ) return stat;

    stat = sendMsg( priv, valueList );
    if ( !( stat & 1 ) ) return stat;

    stat = sendMsg( priv, "\n" );
    if ( !( stat & 1 ) ) return stat;

  }

  return ERR_SUCCESS;

}

int dsWriteOne (
  dsHandle connection,
  int index,
  char *value
) {

privDsHandlePtr priv;
int stat;
char buf[127+1];

  priv = (privDsHandlePtr) connection;
  if ( !priv ) {
    return ERR_BADHANDLE;
  }

  snprintf( buf, 127, "%c%-d,%s\n", 'O', index, value );
  stat = sendMsg( priv, buf );
  if ( !( stat & 1 ) ) return stat;

  return ERR_SUCCESS;

}

int dsFreePvs (
  dsHandle connection
) {

privDsHandlePtr priv;
int stat;

  priv = (privDsHandlePtr) connection;
  if ( !priv ) {
    return ERR_BADHANDLE;
  }

  stat = sendMsg( priv, "F\n" );
  if ( !( stat & 1 ) ) return stat;

  return ERR_SUCCESS;

}

void dsDelay (
  double seconds
) {

struct timeval timeout;

  timeout.tv_sec = (int) floor( seconds );
  timeout.tv_usec = (int) ( ( seconds - floor( seconds ) ) * 1e6 );

  select( FD_SETSIZE, (fd_set *) NULL, (fd_set *) NULL,
   (fd_set *) NULL, &timeout );

}

const char *dsVersion ( void ) {

  return version;

}
