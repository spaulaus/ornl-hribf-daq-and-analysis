#include <stdlib.h>

// ISBYTE - sets a byte in a word to the most significant
//          byte is another word.  This mimcs WTM's routine
void isbyte_(char it[4], char iby[4], int *nb)
{

#ifdef BIGENDIAN
  iby[*nb] = it[3];
#else
  iby[*nb] = it[0];
#endif

  return;
}
