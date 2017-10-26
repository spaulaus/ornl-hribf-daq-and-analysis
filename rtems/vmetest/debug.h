/* The struct with the debugging data */
struct rlvdebug {
  int size;
  int index;
  unsigned short *data;
}; 

/* Initialize the vmedebug structure and allocate space */
void debuginit(void);

/* Clear the debug data space*/
void debugzero(void);

/* Insert value into data */
void debuginsert(short value);

/* Display part of the debug array*/
void debugprint(void);

