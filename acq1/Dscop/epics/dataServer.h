#ifndef __dataServer_h
#define __dataServer_h 1

#define ERR_SUCCESS 1

typedef void *dsHandle;

// Get error message text
char *dsGetErrorText (
  dsHandle connection,
  int errCode
);

// Display error message to stderr
void dsShowError (
  dsHandle connection,
  int errCode
);

// Connect to server and initialize
int dsInit (
  char *addr,          // network address, e.g. "192.168.1.1"
  int port,            // tcp/ip port
  dsHandle *connection // dsInit returns this handle
);

// Clean up and disconnect
int dsTerminate (
  dsHandle connection // from dsInit
);

// Allocate EPICS pvs given in list for write access
int dsAllocPvs (
  dsHandle connection, // from dsInit
  char *pvList         // list of EPICS pv names, e.g. "pv1,pv2,pv3"
);

// Write values to list of pvs; you need not supply values for all pvs
// but pvs are written in the order given in dsAllocPvs
int dsWriteAll (
  dsHandle connection, // from dsInit
  char *valueList      // list of values, e.g. "1.1,2.2,3.3"
);

// Write one value; index corresponds to position of pv in list given
// in dsAllocPvs starting with 0
int dsWriteOne (
  dsHandle connection, // from dsInit
  int index,           // pv name index
  char *value          // value as character string
);

// Deallocate list of EPICS pvs supplied in last dsAllocPvs call
int dsFreePvs (
  dsHandle connection // from dsInit
);

// hi res delay
void dsDelay (
  double seconds
);

// get server/library version string
const char *dsVersion ( void );

#endif
