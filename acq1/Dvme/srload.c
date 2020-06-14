/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1993-2003
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
*    Environment:  VME Acquisition for Linux
*
*    File:         /usr/users/mcsq/Dlinux/Dvme/srload.c
*
*    Description:  Code load standalone programs into the VME processor.
*                  Used to load things like bootstrap loaders and diagnositic
*                  routines.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/ 7/93    MCSQ         Original
*
*    1/20/95    MCSQ         Modified for the new VME boot loader.
*
*    7/28/02    MCSQ         Ported to Linux
*
*    3/19/03    MCSQ         Changed for new pkt_io.c
*
*    3/28/03    MCSQ         Fixed calculation of data length.
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"

#define  MAXC  257


/*    Function Prototypes       */
void sr_write(char *inbuf,int size);
void sr_close(void);
void sr_error(int);

/*
*                      Global variables
*/
char                 in_line[MAXC];      /* line buffer for file input       */
char                 filename[80];       /* filename of program              */
int                  buflen;
unsigned char        *buf_ptr;

FILE *infile;

struct Vmed xbuf,rbuf;

/****************************************************************************
*
*  Command line syntax:
*
*   srload file
*
*  Parameter:   file        - Name of program file to load.  This file must
*                             be S records.
*
****************************************************************************/
main(int argc, char *argv[])
{
   char  *cptr;
   int   size;
   int   reccnt;

   if (argc < 2) {fprintf(stderr,"Need load file name!\n"); exit(1);}
/*
*   Open the input file
*/
   strcpy(filename,argv[1]);
   strcat(filename,".run");
   if ((infile = fopen(filename,"r")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open S record file - %s\n",filename);
       exit(5);
     }
/*
*   Now load the .run file.
*/
   buflen = MAX_ORPH_DATA;
   buf_ptr = (char *)xbuf.buf;
   reccnt=0;
   while (fgets(in_line,MAXC,infile) != NULL)
     {
       ++reccnt;
       size = strlen(in_line);
       sr_write(in_line, size);
       if(!strncmp(in_line,"S9",2)) break;
       printf("Record: %i; Size: %i\n",reccnt, size);
     }
   sr_close();
   pkt_close();
   return(0);
}
/****************************************************************************
*   Pack the output packet and send it when full.
****************************************************************************/
void sr_write(char *buf,int size)
{
   int status;

   printf("sr_write: buflen= %i\n", buflen);
   if (size >= buflen)
     {
       *buf_ptr = '\0';
       rbuf.len = 0;
       xbuf.len = MAX_ORPH_DATA-buflen+1;
       printf("xbuf.len = %i\n",xbuf.len);
       status = pkt_io(&xbuf,&rbuf,CODE,5);
       if (status != 0) sr_error(status);
       buf_ptr = (char *)xbuf.buf;
       buflen = MAX_ORPH_DATA;
     }
   strncpy((char *)buf_ptr,buf,(size_t)size);
   buf_ptr += size;
   buflen -= size;
}
/****************************************************************************
*   Send the last packet.
****************************************************************************/
void sr_close(void)
{
   int status;

   *buf_ptr = '\0';
   rbuf.len = 0;
   xbuf.len = MAX_ORPH_DATA-buflen+1;
   status = pkt_io(&xbuf,&rbuf,CODE,5);
   if (status != 0) sr_error(status);
}
/****************************************************************************
*   Ethernet error
****************************************************************************/
void sr_error(int error)
{
   if (error == ETHER_OPEN) printf("srload: Ethernet open failure.\n");
   if (error == ETHER_TRANSMIT) printf("srload: Ethernet transmit error.\n");
   exit(30);
}
