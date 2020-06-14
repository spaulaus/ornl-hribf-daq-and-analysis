/*======================================================================
   cnaf.c -  Client code to execute a CNAF.  Runs on the host and  
   sends commands to the VME.
   CNAFs come from stdin as C N A F data, one per line.
========================================================================*/
/*    Include files  */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "../Dacq/pkt_io.h"
#include "docnaf.h"
#define LINELENGTH 80

#define F8 8            /* Masks for read, write status of CNAF */
#define F16 16

#define BADOPEN  -34
#define BADWRITE -33
#define BADREAD  -32

#define TRUE  1
#define FALSE 0

/* Prototypes */
int good_cnaf (struct cnafd *);
int display_cnaf(struct cnaf_buffer *,struct cnaf_return *);

/*  CAMAC Data Buffers  */

static struct Vmed xbuf,rbuf;


int main(int argc, char *argv[])  /*  CNAF.C  */
{
   int status;                    /* function status return     */
   int quietflag=0;               /* signal quiet processing    */
   int tmo;                       /* Timeout for cnaf operation */


   struct cnaf_buffer *bcnaf = (struct cnaf_buffer *)xbuf.buf;
   struct cnaf_return *ret_buffer = (struct cnaf_return *)rbuf.buf;

   unsigned char inps[LINELENGTH]; /* input of CNAFs */
   unsigned char error_inps[LINELENGTH];

   char *token;                    /* Used to read stdin tokens */

/*-------------------------------------------------------------------*/

/*    Move past the options to the CNAF, if it exists  */
   while (--argc > 0) {
       if (isdigit(*(++argv)[0]))
          break;
       else if (!strcmp(*argv, "-q")) 
          quietflag = TRUE;
       else if (!strcmp(*argv, "-h")) {
          printf("Usage: docnaf [C N A F [D]] [<filename]\n");
          return 1;
       }
   }


   if (argc == 0) {
/*       Get CNAF lines from stdin  */
/*       It is VERY important NOT to pass NULL pointers to strtol */
      bcnaf->count=0;
      while ((fgets(inps,LINELENGTH,stdin)) != NULL) {

         if (*inps == '#') continue;     /* Comment line, skip */

         strcpy(error_inps, inps);      /* Save the input in case of error */

         if ((token = strtok(inps, " ,")) != NULL)
            bcnaf->cnafs[bcnaf->count].C = strtol(token,NULL,0);

         if ((token = strtok(NULL, " ,")) != NULL)
            bcnaf->cnafs[bcnaf->count].N = strtol(token,NULL,0);

         if ((token = strtok(NULL, " ,")) != NULL)
            bcnaf->cnafs[bcnaf->count].A = strtol(token,NULL,0);

         if ((token = strtok(NULL, " ,")) != NULL)
            bcnaf->cnafs[bcnaf->count].F = strtol(token,NULL,0);

      /* Get data only if F16 set and not F8 - CAMAC write instruction */
         if ((bcnaf->cnafs[bcnaf->count].F & F16) &&
            !(bcnaf->cnafs[bcnaf->count].F & F8)) {

            if ((token = strtok(NULL, " ,")) != NULL)
               bcnaf->cnafs[bcnaf->count].Data = strtol(token,NULL,0);
            else {
               printf("No DATA for write operation.\n   ");
               puts(error_inps);    /* Show the offending line */
               continue;
            }
         }

      /*  Check the CNAF and discard if found wanting */
         if (!good_cnaf(&bcnaf->cnafs[bcnaf->count])) {
            printf ("Bad CNAF line \n    ");
            puts(error_inps);            /* Show the offending line */
            continue;                    /* Skip this line, it's bad */
         }
         ++bcnaf->count;
      }
   }

   else if (argc >= 4) {
/*         Get CNAF's from command line */
      bcnaf->count = 1;
      bcnaf->cnafs[0].C=strtol(*argv++,NULL,0);
      bcnaf->cnafs[0].N=strtol(*argv++,NULL,0);
      bcnaf->cnafs[0].A=strtol(*argv++,NULL,0);
      bcnaf->cnafs[0].F=strtol(*argv++,NULL,0);
/*         Get data only if F16 set and not F8 - CAMAC write instruction */
      if ((bcnaf->cnafs[0].F & F16) &&
          !(bcnaf->cnafs[0].F & F8)) {
           if (argc >= 5) 
              bcnaf->cnafs[0].Data=strtol(*argv++,NULL,0);
           else {
              printf("No DATA for write operation. ");
              printf("Check arguments and retry command.\n");
              return 1;
           }
      }

/*          Check the CNAF and discard if found wanting */
      if (!good_cnaf(&bcnaf->cnafs[0])) {
         printf ("docnaf - Bad CNAF from command line.  Check\n");
         return 1;
      }
      bcnaf->count=1;    /* Only one CNAF from command line allowed */
   }


   else {
      printf("Usage: docnaf [C N A F [D]] [<filename]\n");
      return 1;
   }



/*-------  Send the list and wait for reply  --------------*/

   tmo = 7;       /* 7 seconds */
   xbuf.len = cnaflen(bcnaf);
   rbuf.len = cnaflen(bcnaf);
   status = pkt_io(&xbuf,&rbuf,CNAF, tmo);
   if (status == BADOPEN) {
       printf("docnaf: Unable to open connection\n");
       return 1;
   }
   else if (status == BADWRITE) {
       printf("docnaf: Unable to write CNAF's\n");
       return 1;
   }
   else if (status == BADREAD) {
       printf("docnaf: Unable to get response\n");
       return 1;
   }

/*  Process the buffer of CNAF's */
   if (quietflag != TRUE) 
      display_cnaf(bcnaf, ret_buffer);

   return 0;
}



/*---- display_cnaf ----
  routine to display the returned CNAF's in a pleasant way
*/

int display_cnaf(struct cnaf_buffer *cnafs, struct cnaf_return *result)
                 
{
   int i;

   for (i=0; i<cnafs->count; ++i) {
/*      printf("CNAF executed: CNAF# %i, ",i); */
      printf("(CNAF):(");
      printf("%hi ",cnafs->cnafs[i].C);
      printf("%hi ",cnafs->cnafs[i].N);
      printf("%hi ",cnafs->cnafs[i].A);
      printf("%hi) ",cnafs->cnafs[i].F);
/*      if (result->X == 1 && result->Q == 1) { */
         if (!(cnafs->cnafs[i].F & F16) && !(cnafs->cnafs[i].F & F8)) {
           printf("Data=%i (0x%x) ",result->Data, result->Data);
         }
/*      } */

/*
      printf("X=%hi ",result->X);
      printf("Q=%hi ",result->Q);
      printf("Cok=%hi ", result->Cok);
      printf("Online=%hi ",result->Online);  
*/

      if (result->Online == 0) 
          printf("\n             Crate is OFFLINE ");
      else if (result->X==0) 
          printf("\n             Module does not respond.");
      else if (result->X == 1  && result->Q == 0) 
          printf("\n             Module does not understand");
      else if (result->Cok == 0) 
          printf("\n             No such crate!");
      putchar('\n');
      result++;
   }
   return 0;
}
/*======================================================================
   int good_cnaf (struct cnafd *cnaf)
    checks cnaf for validity.  Checks that 1<=N<=30, 0<=A<=15, 
    0<=F<=31.

=======================================================================*/

int good_cnaf (struct cnafd *cnaf)
{

    if ((cnaf->C < 0) || (cnaf->C >  8))
       return FALSE;
    if ((cnaf->N < 1) || (cnaf->N > 31))
       return FALSE;
    if ((cnaf->A < 0) || (cnaf->A > 15))
       return FALSE;
    if ((cnaf->F < 0) || (cnaf->F > 31))
       return FALSE;

    return TRUE;
}
