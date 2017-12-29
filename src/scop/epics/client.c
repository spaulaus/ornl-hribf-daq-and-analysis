#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "dataServer.h"

int main (
  int argc,
  char **argv
) {

int i, ii, port, stat;
dsHandle connection;
int iv1 = 0;
double dv1 = 0, dv2 = 0;
char buf[127+1];

  if ( argc < 3 ) {
    fprintf( stderr, "dslib version %s\n", dsVersion() );
    fprintf( stderr, "usage: %s <ip addr> <port>\n", argv[0] );
    return -1;
  }

  port = (int) atol( argv[2] );

  stat = dsInit( argv[1], port, &connection );
  if ( !( stat & 1 ) ) {

    // Next two lines are equivalent
    fprintf( stderr, "dslib: %s\n", dsGetErrorText( connection, stat ) );
    dsShowError( connection, stat );

    dsTerminate( connection );
    exit( -1 );

  }

  stat = dsAllocPvs( connection,
   "scal:bl21:cnt0,scal:rms:cnt0,scal:drs:cnt0" );
  if ( !( stat & 1 ) ) {
    dsShowError( connection, stat );
    dsTerminate( connection );
    exit( -1 );
  }

  for ( i=0; i<6; i++ ) {

    for ( ii=0; ii<5; ii++ ) {

      iv1 += 1;
      dv1 += 0.1;
      dv2 += 0.12;

      sprintf( buf, "%-d,%lf,%lf", iv1, dv1, dv2 );

      // this sets pv0=1, pv1=2.1, pv2=3.2
      stat = dsWriteAll( connection, buf );
      if ( !( stat & 1 ) ) {
        dsShowError( connection, stat );
        dsTerminate( connection );
        exit( -1 );
      }

      dsDelay( 0.2 );

    }

    for ( ii=0; ii<5; ii++ ) {

      iv1 -= 1;
      dv1 -= 0.1;
      dv2 -= 0.12;

      sprintf( buf, "%-d,%-f,%-f", iv1, dv1, dv2 );

      // this sets pv0=1, pv1=2.1, pv2=3.2
      stat = dsWriteAll( connection, buf );
      if ( !( stat & 1 ) ) {
        dsShowError( connection, stat );
        dsTerminate( connection );
        exit( -1 );
      }

      dsDelay( 0.2 );

    }

  }

  // this sets pv0=1, pv1=2.1, pv2 is unchanged
  stat = dsWriteAll( connection, "1,2.1" );
  if ( !( stat & 1 ) ) {
    dsShowError( connection, stat );
    dsTerminate( connection );
    exit( -1 );
  }

  dsDelay( 0.1 );

  // this sets pv0=1
  stat = dsWriteOne( connection, 0, "1" );
  if ( !( stat & 1 ) ) {
    dsShowError( connection, stat );
    dsTerminate( connection );
    exit( -1 );
  }

  dsDelay( 0.1 );

  // this sets pv1=2.1
  stat = dsWriteOne( connection, 1, "2.1" );
  if ( !( stat & 1 ) ) {
    dsShowError( connection, stat );
    dsTerminate( connection );
    exit( -1 );
  }

  dsDelay( 0.1 );

  // this sets pv2=3.2
  stat = dsWriteOne( connection, 2, "3.2" );
  if ( !( stat & 1 ) ) {
    dsShowError( connection, stat );
    dsTerminate( connection );
    exit( -1 );
  }

  dsDelay( 1.0 );

  stat = dsFreePvs( connection );
  if ( !( stat & 1 ) ) {
    dsShowError( connection, stat );
    dsTerminate( connection );
    exit( -1 );
  }

  dsDelay( 1.0 );

  stat = dsTerminate( connection );
  if ( !( stat & 1 ) ) {
    dsShowError( connection, stat );
    exit( -1 );
  }

  exit( 0 );

}
