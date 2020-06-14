/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1995
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
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dvme3/vmeload.c
*
*    Description:  Routine to load a module into the VME processor.  This
*                  works only for modules which have no external references
*                  or absolute sections.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/12/95    MCSQ         Original
*
*    6/ 6/95    MCSQ        Fixed to work with version of OASYS stuff
*                           on our Alphas.  Changes are backward compatible
*                           with the DECstations.
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*
*   Global variables
*/

#define MAXC 4096
unsigned char in_buf[MAXC];
struct record {
     unsigned char len;
     unsigned char cksum;
              char type;
              char filler;
unsigned char data[252];
} rec_buf;

struct reloc {
  unsigned char *wksmem;
            int  vmemem;
            int  size;
} sections[16];

FILE *infile, *outfile;

char inname[80];
char outname[80];

/*
 *  Function Prototypes
 */
unsigned int get_long(unsigned char *);
void put_long(unsigned char *,unsigned int);
void put_word(unsigned char *,unsigned int);
int  load_dat(int,unsigned char *);
void write_sr(int);
void write_start(unsigned int);
char *hex_ascii(char *,unsigned char, unsigned char *);

/******************************************************************************
*****************************************************************************/
main(int argc, char *argv[])
{
   int bcount = 0, rcount = 0, rlen = 0,i,sum;
   unsigned int tmp,size,start;
   unsigned char *iptr, *rbuf;

   if (argc < 4)
    {
      fprintf(stderr,"Usage:  vmeload input_file output_file load_address\n");
      exit(1);
    }
/**********
printf("%s %s %s %s\n",argv[0],argv[1],argv[2],argv[3]);
**********/
/*
*   Open the input file
*/
   strcpy(inname,argv[1]);
   strcat(inname,".bin");
   if ((infile = fopen(inname,"r")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open input file - %s\n",inname);
       exit(2);
     }
/*
*   Get load address from command line
*/
   if (sscanf(argv[3],"%x",&sections[0].vmemem) != 1)
     {
       exit(4);
     }
/*
*    Read input file to get section sizes.  If there are any record
*    types we cannot handle, just exit.
*/
   while(1)
     {
       if (bcount == 0)
         {
           bcount = fread(in_buf,1,sizeof(in_buf),infile);
           if (bcount <= 0) break;
           iptr = in_buf;
         }
       if (rcount == 0)
         {
           rlen = *iptr;
           rcount = 0;
           rbuf = (unsigned char *)&rec_buf;
         }
       *rbuf++ = *iptr++;
       ++rcount;
       --bcount;
       if (rcount == rlen)
         {
           sum = 0;
           rbuf = (unsigned char *)&rec_buf;
           for(i=0; i < rec_buf.len; i++) sum += *rbuf++;
           if ((sum & 0xff) != 0)
             {
               printf("checksum error = %x\n",sum);
               exit(5);
               break;
             }
           switch (rec_buf.type)
             {
               case  5:
                 printf("named section record\n");
                 exit(8);
                 break;
               case  6:
                 printf("debug record\n");
                 exit(9);
                 break;
               case  7:
                 i = rec_buf.data[0] & 0xf;
                 if (i == 0)
                   {
                     printf("Absolute section\n");
                     exit(10);
                   }
                 size = get_long(&rec_buf.data[1]);
                 sections[i].size += size;
                 break;
               case  8:
                 printf("debug file record\n");
                 exit(10);
                 break;
               case  9:
                 printf("module record\n");
                 exit(11);
                 break;
               default:
                 break;
             }
           rcount = 0;
         }
      }
   rewind(infile);
/*
*   Compute number of bytes required to store the VME data.  Allocate
*   the required memory and setup the section table.
*/
   size = 0;
   for (i=0; i < 16; i++) size += sections[i].size;
   iptr = (unsigned char *)malloc(size);
   tmp = sections[0].vmemem;
   for (i=0; i < 16; i++)
     {
       sections[i].vmemem = tmp;
       sections[i].wksmem = iptr;
       tmp = sections[i].vmemem + sections[i].size;
       iptr = sections[i].wksmem + sections[i].size;
     }
   bcount = 0;
   rcount = 0;
   rlen = 0;
/*
*    Read and process the input file data records
*/
   while(1)
     {
       if (bcount == 0)
         {
           bcount = fread(in_buf,1,sizeof(in_buf),infile);
           if (bcount <= 0) break;
           iptr = in_buf;
         }
       if (rcount == 0)
         {
           rlen = *iptr;
           rcount = 0;
           rbuf = (unsigned char *)&rec_buf;
         }
       *rbuf++ = *iptr++;
       ++rcount;
       --bcount;
       if (rcount == rlen)
         {
           sum = 0;
           rbuf = (unsigned char *)&rec_buf;
           for(i=0; i < rec_buf.len; i++) sum += *rbuf++;
           if ((sum & 0xff) != 0)
             {
               printf("checksum error = %x\n",sum);
               exit(5);
               break;
             }
           switch (rec_buf.type)
             {
               case  0:
/*
*   Open the output file
*/
                 strcpy(outname,argv[2]);
                 strcat(outname,".run");
                 if ((outfile = fopen(outname,"w")) == (FILE *)NULL)
                   {
                     fprintf(stderr,"Cannot open output file - %s\n",outname);
                     exit(3);
                   }
                 i = rec_buf.data[0] & 0xf;
                 start = get_long(&rec_buf.data[1]);
                 if ((rec_buf.data[0] & 0x10) != 0)
                   {
                     start = start + sections[i].vmemem;
                   }
                 write_sr(9);
                 write_sr(13);
                 write_sr(15);
                 write_start(start);
                 break;
               case  1:
                 if (load_dat(rec_buf.len-4,rec_buf.data))
                   {
                     printf("data record error\n");
                     exit(6);
                   }
                 break;
               case  2:
                 i = rec_buf.len - 4;
                 rec_buf.data[i] = '\0';
                 if ((rec_buf.data[0] & 0x20) != 0)
                   {
                     printf("extern sym = %s\n",&rec_buf.data[7]); 
                     exit(7);
                   }
                 break;
               default:
                 break;
             }
           rcount = 0;
         }
      }
/*********************
for(i=0; i < 16; i++)
  {
    if (sections[i].size != 0)
      {
        printf("sec = %i wksmem = %x, vmemem = %x, size = %i\n",
               i,sections[i].wksmem,sections[i].vmemem,sections[i].size);
      }
  }
*********************/
   return(0);
}
/******************************************************************************
*   Get a long word (32-bits) from the input stream
*****************************************************************************/
unsigned int get_long(unsigned char *uptr)
{
   unsigned int tmp;

   tmp = *uptr++;
   tmp = tmp << 8;
   tmp += *uptr++;
   tmp = tmp << 8;
   tmp += *uptr++;
   tmp = tmp << 8;
   tmp += *uptr;
   return (tmp);
}
/******************************************************************************
*   Put a long word (32-bits) into the data section
*****************************************************************************/
void put_long(unsigned char *uptr, unsigned int val)
{
   uptr += 3;
   *uptr-- = val;
   val = val >> 8;
   *uptr-- = val;
   val = val >> 8;
   *uptr-- = val;
   val = val >> 8;
   *uptr = val;
}
/******************************************************************************
*  Put a word (16-bits) into the data section
*****************************************************************************/
void put_word(unsigned char *uptr, unsigned int val)
{
   *(uptr+1) = val;
   val = val >> 8;
   *uptr = val;
}
/******************************************************************************
*  Process a load record
*****************************************************************************/
int load_dat(int len,unsigned char *uptr)
{
   unsigned char *optr;
   int  msk,modflg,relb,s,sec,offset;
   int  rel_long = 0, count = 0;
   unsigned int tmp = 0;

#ifdef  DEBUG
printf("len = %x\n",len);
#endif
   sec = *uptr++ & 0xf;
   offset = get_long(uptr);
   optr = sections[sec].wksmem + offset;
#ifdef  DEBUG
printf("sec = %x\n",sec);
printf("offset = %x\n",offset);
printf("optr = %x\n",optr);
#endif
   uptr += 4;
   len -= 5;
   while(len)
     {
       modflg = *uptr++;
#ifdef  DEBUG
printf("\nmodflg = %x\n",modflg);
#endif
       --len;
       msk = 1;
       for(; msk < 256 && len > 0;)
         {
#ifdef  DEBUG
printf("%2.2x ",*uptr);
#endif
           if (count)
             {
               tmp = tmp << 8;
               tmp = tmp | *uptr++;
               --count;
               if (count == 0)
                 {
                   tmp += sections[s].vmemem;
#ifdef  DEBUG
printf("tmp = %x\n",tmp);
#endif
                   if (rel_long)
                     {
                       put_long(optr,tmp);
                       optr += 4;
                     }
                   else
                     {
                       put_word(optr,tmp);
                       optr += 2;
                     }
                 }
               msk = msk << 1;
             }
           else if ((modflg & msk) == 0)
             {
#ifdef  DEBUG
printf("%2.2x ",*uptr);
#endif
               *optr++ = *uptr++;
               msk = msk << 1;
             } 
           else
             {
                relb = *uptr++;
#ifdef  DEBUG
printf("\nrelb = %x\n",relb);
#endif
                s = relb & 0xf;
                if (s == 0) return(1);
                tmp = 0;
                if (relb & 0xa0) return(1);
                if (relb & 0x40)
                  {
                    count = 4;
                    rel_long = 1;
                  }
                else
                  {
                    count = 2;
                    rel_long = 0;
                  }
             }
           --len;
         }
     }
  return(0);
}
/******************************************************************************
*   Convert a data section to S records and output to the output file
******************************************************************************/
void write_sr(int sec)
{
   unsigned char cksum,tmp;
            char oline[96];
   unsigned int  addr;
   unsigned int  maxaddr;
   unsigned char *ucptr;
            char *optr;
            int  count;

   if (sections[sec].size == 0) return;
   addr = sections[sec].vmemem;
   maxaddr = addr + sections[sec].size;
   ucptr = sections[sec].wksmem;
   while (addr < maxaddr)
    {
      optr = oline;
      *optr++ = 'S';
      *optr++ = '3';
      count = maxaddr - addr + 1;
      if (count > 32) count = 32;
      cksum = 0;
      optr = hex_ascii(optr,(unsigned char)(count+5),&cksum);
      tmp = addr >> 24;
      optr = hex_ascii(optr,tmp,&cksum);
      tmp = addr >> 16;
      optr = hex_ascii(optr,tmp,&cksum);
      tmp = addr >> 8;
      optr = hex_ascii(optr,tmp,&cksum);
      tmp = addr & 0xff;
      optr = hex_ascii(optr,tmp,&cksum);
      while (count)
       {
         optr = hex_ascii(optr,*ucptr++,&cksum);
         addr += 1;
         count--;
       }
      cksum = ~cksum;
      optr = hex_ascii(optr,cksum,&cksum);
      *optr++ = '\n';
      *optr++ = 0;
      fwrite(oline,(size_t)(sizeof(char)),(size_t)strlen(oline),outfile);
    }
}
/******************************************************************************
*   Build the start S record and output to the ourput file
******************************************************************************/
void write_start(unsigned int start)
{
   unsigned char cksum,tmp;
            char oline[96];
            char *optr;

   optr = oline;
   *optr++ = 'S';
   *optr++ = '8';
   cksum = 0;
   optr = hex_ascii(optr,(unsigned char)(4),&cksum);
   tmp = start >> 16;
   optr = hex_ascii(optr,tmp,&cksum);
   tmp = start >> 8;
   optr = hex_ascii(optr,tmp,&cksum);
   tmp = start & 0xff;
   optr = hex_ascii(optr,tmp,&cksum);
   cksum = ~cksum;
   optr = hex_ascii(optr,cksum,&cksum);
   *optr++ = '\n';
   *optr++ = 0;
   fwrite(oline,(size_t)(sizeof(char)),(size_t)strlen(oline),outfile);
}
/******************************************************************************
 *   Convert byte to two ASCII characters
******************************************************************************/
char *hex_ascii(char *str,unsigned char byte, unsigned char *sum)
{
   unsigned char nibble;

   nibble = byte >> 4;
   if (nibble < 10) nibble = nibble + 0x30;
   else  nibble = nibble + 0x37;
   *str++ = nibble;
   nibble = byte & 0xf;
   if (nibble < 10) nibble = nibble + 0x30;
   else  nibble = nibble + 0x37;
   *str++ = nibble;
   *sum += byte;
   return (str);
}
