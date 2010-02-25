
#include "vme_sys.h"
#include "caen.h"

main()
{
  short tmp1;
  struct CAEN *ptr = (struct CAEN *)CAEN785_1;

  while(1) {tmp1 = ptr->status_reg2;}
}
