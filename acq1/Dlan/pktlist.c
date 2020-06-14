/*=================================================================*/
/*    pktlist.c 
      Routines to maintain and manipulate a list of packet 
      identification information received.  It is used to reject
      retransmissions because of delays.
      Robert Varner
===================================================================*/
/*
*   revisions:
*
*   9/20/95  MCSQ   Reduced MAXRECORDS from 16 to 2.  Also changed
*                   list algorithm.  In time I may reduce MAXRECORDS
*                   to 1 and then put all his in-line.
*
*/
#include <stdio.h>
#include <sys/time.h>
#include <string.h>
#include "orph_pf.h"

#define MAXRECORDS 2
#define TRUE 1
#define FALSE 0

static struct pkt_record {
       unsigned int  request_number;
       unsigned char order;
       unsigned char source[6];
} key[MAXRECORDS];         /* Records of previous packets */

static int  next;

void en_InitRecord(void)
{
     next = 0;
     return;
}
int en_FindPacket(struct Ether_Packet *tst)
{
    int i,j;

    i = next;
    for (j = MAXRECORDS; j != 0; j--)
      { 
        i--;
        if (i < 0) i = MAXRECORDS - 1;
        if (key[i].order != tst->Order) continue;
        if (key[i].request_number != tst->Request_Number) continue;
        if (memcmp(tst->Source, key[i].source, HW_ADDR_LEN) == 0) return TRUE;
      }
    key[next].order = tst->Order;
    key[next].request_number = tst->Request_Number;
    memcpy(key[next].source,tst->Source,HW_ADDR_LEN);
    next++;
    next = next % MAXRECORDS;
    return FALSE;
}
void en_dump_records_(void)
{
    int i,j,k;

    i = next;
    for (j = MAXRECORDS; j != 0; j--)
      { 
        i--;
        if (i < 0) i = MAXRECORDS - 1;
        fprintf(stderr,"Rec: Seq= %2x, Req= %i, Src = ",
                               key[i].order,key[i].request_number);
        for(k=0; k < HW_ADDR_LEN-1; k++)
                                     fprintf(stderr,"%2.2x-",key[i].source[k]);
        fprintf(stderr,"%2.2x\n",key[i].source[HW_ADDR_LEN-1]);

      }
    return;
}
