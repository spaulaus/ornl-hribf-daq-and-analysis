/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1993
*
*
*
*                         IMPORTANT NOTICE TO USER
*
*     All statements, technical information and recommendations contained
*     herein are based on tests we believe to be reliable, but the accuracy
*     or completeness thereof is not guaranteed, and the following  is
*     made in lieu of all warranties expressed or implied:
*
*     Our only obligation shall be to correct or replace, at our convenience,
*     any part of the product proven to be defective.  We shall not be liable
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  ORPHAS Acquisition System
*
*    File:         /usr/users/mcsq/Dwks/get_acq_key.c
*
*    Description:  
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/ 8/93    MCSQ         Original
*****************************************************************************/

#include <string.h>
#include <ctype.h>
#include <sys/types.h>

#define  ACQ
#include "../Dshm/ipckeys.h"

int get_acq_key(char *device,int len,struct acq_keys *key,char ***resource)
{

   int  i,stat = 1;
   char  name[12];

/*
*   Convert the FORTRAN character variable to a null terminated
*   lower case string.
*/
   if (len >= sizeof(name)) len = sizeof(name)-1;
   else if (len < 0) len =0;
   while(len > 0)
     {
       if (device[len-1] != ' ') break;
       --len;
     }
   strncpy(name,device,len);
   name[len] = '\0';
   for (i=0; i < len; i++) name[i] = tolower(name[i]);

   i = 0;
/*
*   Search the device table for the specified device.  If found,
*   we return the keys for this device in the struct keys.
*   Keys for the devices are defined in the include file
*   'ipckeys.h'.
*/
   while (acq_dev[i].name != NULL)
     {
       if (!strcmp(name,acq_dev[i].name))
        {
          stat = 0;
          break;
        }
       i++;
     }
   *key = acq_dev[i].keyval;
   if (resource != NULL) *resource = acq_res;
   return stat;
}
