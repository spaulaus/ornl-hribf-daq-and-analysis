/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1993-2000
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
*    File:         /usr/users/mcsq/Dwks/pktproc.c
*
*    Description:  
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/22/93    MCSQ         Original
*
*    3/30/00    MCSQ         Handle loss of multiple packets for continuation
*                            packet sets.
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <inttypes.h>
#include "orph_udp.h"
#include "orpas_data.h"
#include "acqshm.h"
#include "acqlib.h"

/*  Prototypes of functions in udptoipc.c   */
int split_pkt(unsigned char *,int *,int,int);
int FindEventEnd(unsigned short *, int);

char mess[78];
struct shm_buf *curbuf,*nextbuf,*tmpbuf;
unsigned char *datptr;
int max_buf;
char savbuf[PKTHDRLEN+DATAHDRSZ];

struct UDP_Packet *pktprocess(struct UDP_Packet *pkt_buf,int buflen)
{
   enum typepkt {REG,STOP,FCONT,CCONT,LCONT,BAD};

    static enum typepkt thispkt;
             static int expcont = 0;
             static int used = 0;
   static unsigned char *contptr;
             static int contused = 0;
    static unsigned int contevt;
      struct orpas_data *en_buf;
          unsigned char *cptr;
                    int  evtnum,numevts,conf;
                    int  error,size;

/*
*  Save header info in the data portion of the received packet.  This
*  consists of the event number of the first event in the packet, the
*  number of events in the packet and the continuation flag.
*/
   en_buf = (struct orpas_data *)pkt_buf->Data;
   numevts = en_buf->NumberEvents;
   conf  = en_buf->Continuation;
   evtnum = en_buf->TotalEvents;
/*
*  Determine from the header flags what type of packet this is.
*/
   if (conf == 0)
     {
       if (numevts == 0) thispkt = STOP;
       else  thispkt = REG;
     }
   else
     {
       if (conf == 1 && numevts == 0) thispkt = FCONT;
       else if (conf > 1 && numevts == 1) thispkt = LCONT;
       else if (conf > 1 && numevts == 0) thispkt = CCONT;
       else  thispkt = BAD;
     }
/*
*  Check for start of continuation or a break in seqence of
*  a continuation packet set.
*/
   if (conf != expcont)
     {
       if (expcont)
         {
/*
*  Must have lost a packet in the continuation set.
*/
           if (thispkt == STOP)
             {
               datptr = contptr;
               used = contused;
               expcont = 0;
             }
           else thispkt = BAD;
         }
       else if (thispkt != FCONT) thispkt = BAD;
       if (thispkt == FCONT)
         {
/*
*  This is the first packet of a continuation packet set.
*/
           contptr = datptr;
           contused = used;
           contevt = evtnum;
           expcont = 2;
         }
     }
   else if (expcont) expcont++;

/*
 *   Check size of continuation packet set.  If it is too large,
 *   discard the entire continuation set.
 */
   
   if (thispkt == CCONT || thispkt == LCONT)
     {
       if ((datptr - contptr + buflen) > max_buf)
	 {
	   thispkt = BAD;
	   printf("\7Too many continuation packets!\n");
	   printf("\7Data are being discarded\n");
	 }
     }
   /*
   *   Another check on continuation packets.  When the first packet of a
   *   continuation is received, the event number is saved.  All remaining
   *   packets of the continuation set must have the same event number.
   */
   if ((thispkt == CCONT || thispkt == LCONT) && contevt != evtnum)
     thispkt = BAD;

   if (thispkt == BAD)
     {
/*
*   This packet has a bad header or we have lost parts of a set of
*   continuation packets.  Junk this one and go for more gusto.
*/
       if (expcont)
         {
           datptr = contptr;
           used = contused;
           expcont = 0;
           pkt_buf = (struct UDP_Packet *)(datptr - DATAHDRSZ - PKTHDRLEN);
           memcpy(savbuf,(char *)pkt_buf,DATAHDRSZ+PKTHDRLEN);
	   //rlv
	   pkt_buf->DataSize=MAX_ORPH_DATA;
        }
       return pkt_buf;
     }

   /* Restore the shm buffer segment overwritten by spkt_recv */
   memcpy(pkt_buf,savbuf,DATAHDRSZ+PKTHDRLEN);

   if (!used) curbuf->event_num = evtnum;

/*
*  Determine the real data length of the packet (bytes).
*/
   if (buflen == MIN_ORPH_DATA)
     {
       if (thispkt == STOP) buflen = 0;
       else  buflen = FindEventEnd(&en_buf->Buffer,
                       (int)((long)buflen-offsetof(struct orpas_data,Buffer)));
     }
   else   buflen = buflen - DATAHDRSZ;
   size = used + buflen;
   if (thispkt == STOP)
     {
       if (used == 0) return pkt_buf;
/*
*  Write whatever is in the buffer on the first STOP packet.
*/
       memset(datptr,0xff,max_buf-used);
       used = 0;
     }
   else if (size < (max_buf - 80))
     {
/*
*  Accept another packet
*/
       curbuf->events += numevts;
       used = size;
       datptr = datptr + buflen;
       cptr = datptr - DATAHDRSZ - PKTHDRLEN;
       memcpy(savbuf,cptr,DATAHDRSZ+PKTHDRLEN);
       //rlv
       ((struct UDP_Packet *)cptr)->DataSize=MAX_ORPH_DATA;
       if (thispkt == LCONT) expcont = 0;
       return  (struct UDP_Packet *)cptr;
     }
   if (expcont == 0)
     {
/*
*   This packet is not a FCONT, CCONT or a LCONT
*/
       if (size > max_buf)
         {
            int evts = numevts;
            int len;

            len = split_pkt(datptr,&evts,buflen,max_buf-used);
            datptr += len;
            memcpy(nextbuf->data,datptr,buflen-len);
            memset(datptr,0xff,max_buf-used-len);
            used = buflen - len;
            curbuf->events += evts;
            nextbuf->event_num = evtnum + evts;
            nextbuf->events = numevts-evts;
         }
       else
         {
           curbuf->events += numevts;
           datptr = datptr + buflen;
           memset(datptr,0xff,max_buf-size);
           used = 0;
           nextbuf->events = 0;
         }
     }
   else
     {
/*
*   This packet is some kind of continuation packet.
*/
       if (size <= max_buf && thispkt == LCONT)
         {
           datptr = datptr + buflen;
           memset(datptr,0xff,max_buf-size);
           used = 0;
         }
       else
         {
           if (contused == 0 && thispkt != LCONT)
             {
               used = size;
               datptr = datptr + buflen;
               cptr = datptr - DATAHDRSZ - PKTHDRLEN;
               memcpy(savbuf,cptr,DATAHDRSZ+PKTHDRLEN);
	       //rlv
	       ((struct UDP_Packet *)cptr)->DataSize=MAX_ORPH_DATA;
               return  (struct UDP_Packet *)cptr;
             }
           else if (contused == 0 && thispkt == LCONT)
             {
               curbuf->size = size;
               used = 0;
             }
           else
             {
               used = size - contused;
               memcpy(nextbuf->data,contptr,used);
               memset(contptr,0xff,max_buf-contused);
               contused = used;
               contptr =  (unsigned char *)nextbuf->data;
               nextbuf->event_num = evtnum;
             }
         }
       nextbuf->events = 0;
     }
/*
*   Mark this buffer available to consumers and get a pointer to another
*   buffer.
*/
   tmpbuf = write_shm(curbuf,&error);
   if (error != 0)
     {
       acq_error(&error,mess,sizeof(mess));
       printf("%s\n");
       exit(99);
     }
   curbuf = nextbuf;
   nextbuf = tmpbuf;
   max_buf = Shm->buf_size;
/*
*   If the buffer size has been reduced, what is leftover may be larger
*   than the new buffer size.  If so, just write it and get a new buffer.
*/
   if (used > max_buf)
     {
       curbuf->size = used;
       used = 0;
       tmpbuf = write_shm(curbuf,&error);
       if (error != 0)
         {
           acq_error(&error,mess,sizeof(mess));
           printf("%s\n");
           exit(99);
         }
       curbuf = nextbuf;
       nextbuf = tmpbuf;
     }
   datptr = (unsigned char *)curbuf->data + used;
   cptr = datptr - DATAHDRSZ - PKTHDRLEN;
   memcpy(savbuf,cptr,DATAHDRSZ+PKTHDRLEN);
   ((struct UDP_Packet *)cptr)->DataSize = MAX_ORPH_DATA; // needed in pkt_recv
   if (thispkt == LCONT) expcont = 0;
   return  (struct UDP_Packet *)cptr;
}
/*****************************************************************************
*****************************************************************************/
void buffers_init(void)
{
   int  status;
   extern struct shm_use *Shm;
   extern struct UDP_Packet *pkt_buf;

   max_buf = Shm->buf_size;
   curbuf = write_shm(NULL,&status);
   if (status != 0)
     {
       acq_error(&status,mess,sizeof(mess));
       printf("%s\n");
       exit(99);
     }
   datptr = (unsigned char *)curbuf->data;
   curbuf->event_num = 0;
   curbuf->events = 0;
   nextbuf = write_shm(NULL,&status);
   if (status != 0)
     {
       acq_error(&status,mess,sizeof(mess));
       printf("%s\n");
       exit(99);
     }
   nextbuf->events = 0;
   pkt_buf = (struct UDP_Packet *)(((char *)curbuf->data)-DATAHDRSZ-PKTHDRLEN);
   memcpy(savbuf,pkt_buf,DATAHDRSZ+PKTHDRLEN);
   //rlv
   pkt_buf->DataSize = MAX_ORPH_DATA;
}
