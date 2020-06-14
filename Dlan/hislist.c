/******************************************************************************
*    hislist.c 
*  Save a list of received and tramsmitted packets.  Only a small part
*  of the packet header data is saved.  This, however, should be plenty
*  for determining the flow of packets between the host and VME system.
*
*  9/20/95  MCSQ
******************************************************************************/
#include <stdio.h>
#include <sys/time.h>
#include <string.h>
#include <time.h>
#include "orph_pf.h"

#define MAXRECORDS 16
#define TRUE 1
#define FALSE 0

static struct his_record {
       time_t        time;
       unsigned char rtype;
       unsigned char ack;
       unsigned char source[6];
       unsigned char order;
       unsigned int  request_number;
} key[MAXRECORDS];         /* Records of previous packets */

static int  next;

/*****************************************************************************
*    Initialize the history list
*****************************************************************************/
void en_InitHis(void)
{
     int i;

     next = 0;
     for (i=0; i<MAXRECORDS; i++) key[i].rtype = 0;
     return;
}
/*****************************************************************************
*    Add a packet to the history list
*****************************************************************************/
void en_AddHis(struct Ether_Packet *new, int type)
{
     key[next].time = time(NULL);
     key[next].rtype = type;
     key[next].ack = new->Ack;
     key[next].order = new->Order;
     if (type == RECI_PKT)
       {
         key[next].request_number = new->Request_Number;
         memcpy(key[next].source,new->Source,HW_ADDR_LEN);
       }
     next++;
     next = next % MAXRECORDS;
     return;
}
/*****************************************************************************
*    Display the history list on the STDERR device.
*****************************************************************************/
void en_dump_his_(void)
{
    int i,j,k;
    char *cptr;

    i = next;
    for (j = MAXRECORDS; j != 0; j--)
      { 
        i--;
        if (i < 0) i = MAXRECORDS - 1;
        if (key[i].rtype == 0) continue;
        cptr = ctime(&key[i].time);
        *(cptr+24) = '\0';
        if (key[i].rtype == RECI_PKT)
          {
            fprintf(stderr,"%s -  Rec: Seq= %2x, Ack= %2x, Req= %i, Src= ",cptr,
                               key[i].order,key[i].ack,key[i].request_number);
            for(k=0; k < HW_ADDR_LEN-1; k++)
                                      fprintf(stderr,"%2.2x-",key[i].source[k]);
            fprintf(stderr,"%2.2x\n",key[i].source[HW_ADDR_LEN-1]);
          }
        else
          {
            fprintf(stderr,"%s - Xmit: Seq= %2x, Ack= %2x\n",cptr,
                                                    key[i].order,key[i].ack);
          }
        fflush(stderr);
      }
/**********
    en_InitHis();
**********/
    return;
}
