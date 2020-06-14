/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1995
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
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dvme3/combine.c
*
*    Description:  Routine to combine multiple xxx.run files into a
*                  single yyy.run file.  Used to combine the various
*                  parts of the VME operating system into a single
*                  file for booting the VME processor.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    3/26/92    MCSQ         
*
*    2/14/95    MCSQ       Complete re-write of code.  
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAXC 257
#define HEXSIZE 131072

int  i;
unsigned int  minaddr = 0xffffffff,maxaddr = 0;
char line[MAXC],filename[40];
unsigned char hex_array[HEXSIZE];

FILE *infile, *outfile;

/*   Function Prototypes    */
void readit(void);
unsigned char ascii_hex(char *,int *,int *);
void writeit(void);
char *hex_ascii(char *,unsigned char , unsigned char *);

main(int argc, char *argv[])
{
   if (argc < 3) {fprintf(stderr,"Need input and output file names!\n"); exit(1);}
/*
*   Open the output file
*/
   strcpy(filename,argv[1]);
   strcat(filename,".run");
   if ((outfile = fopen(filename,"w")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open output file - %s\n",filename);
       exit(3);
     }

   i = 2;
   while(i < argc)
    {
/*
*   Open the input file
*/
      strcpy(filename,argv[i]);
      strcat(filename,".run");
      if ((infile = fopen(filename,"r")) == (FILE *)NULL)
        {
          fprintf(stderr,"Cannot open input file - %s\n",filename);
          exit(2);
        }
      readit();
      i++;
    }
   writeit();
   fwrite(line,(size_t)(sizeof(char)), (size_t)strlen(line), outfile);
   return(0);
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$      readit     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*
*   Only S1, S2 and S3 records are decoded.
*
*/

void readit(void)
{
   unsigned char   bytes;
   unsigned int    addr,temp;
   char            *cptr;
   int             err,cksum,recnum=0;

/*
*  Read input file
*/
   while((cptr = fgets(line,sizeof(line),infile)) != (char *) NULL)
   {
     while (*cptr == ' ' && cptr - line < sizeof(line)) ++cptr;
     if (cptr - line == sizeof(line)) continue;
     if (strncmp(cptr,"S9",2) == 0) return;
     else if (strncmp(cptr,"S8",2) == 0) return;
     else if (strncmp(cptr,"S7",2) == 0) return;
     cksum = 0;
     err = 0;
     if (strncmp(cptr,"S2",2) == 0)
     {
       cptr += 2;
       bytes = ascii_hex(cptr,&cksum,&err) - 4;
       cptr += 2;
       addr = ascii_hex(cptr,&cksum,&err);
       cptr += 2;
       addr = (addr << 8) + ascii_hex(cptr,&cksum,&err);
       cptr += 2;
       addr = (addr << 8) + ascii_hex(cptr,&cksum,&err);
       cptr += 2;
     }
     else if (strncmp(cptr,"S3",2) == 0)
     {
       cptr += 2;
       bytes = ascii_hex(cptr,&cksum,&err) - 5;
       cptr += 2;
       addr = ascii_hex(cptr,&cksum,&err);
       cptr += 2;
       addr = (addr << 8) + ascii_hex(cptr,&cksum,&err);
       cptr += 2;
       addr = (addr << 8) + ascii_hex(cptr,&cksum,&err);
       cptr += 2;
       addr = (addr << 8) + ascii_hex(cptr,&cksum,&err);
       cptr += 2;
     }
     else if (strncmp(cptr,"S1",2) == 0)
     {
       cptr += 2;
       bytes = ascii_hex(cptr,&cksum,&err) - 3;
       cptr += 2;
       addr = ascii_hex(cptr,&cksum,&err);
       cptr += 2;
       addr = (addr << 8) + ascii_hex(cptr,&cksum,&err);
       cptr += 2;
     }
     else
       continue;
     if (addr < minaddr) minaddr = addr;
     temp = addr + bytes - 1;
     if (temp > maxaddr) maxaddr = temp;
     while (bytes > 0 && addr <= maxaddr)
       {
	 hex_array[addr] = ascii_hex(cptr,&cksum,&err);
	 cptr += 2;
	 bytes -= 1;
 	 addr += 1;
       }
     ascii_hex(cptr,&cksum,&err);
     recnum++;
     if ((cksum & 0xff) != 0xff)
     {
       fprintf(stderr,"Checksum error in S record number %i\n",recnum);
       exit(0xfd);
     }
     if (err != 0)
     {
       fprintf (stderr,"Non-hex character in S record!\n<%s>\n",line);
       exit (0xfe);
     }
   }
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     ascii_hex   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Convert ASCII hex to a byte
*/
unsigned char ascii_hex(char *str,int *sum,int *err)
{
   int  nibble;
   unsigned char byte;

   nibble = *str++ - 0x30;
   if (nibble < 0) *err = 1;
   if (nibble > 9) nibble -= 7;
   if (nibble > 0xf) *err = 1;
   byte = nibble << 4;
   nibble = *str++ - 0x30;
   if (nibble < 0) *err = 1;
   if (nibble > 9) nibble -= 7;
   if (nibble > 0xf) *err = 1;
   *sum += (byte + nibble);
   return (byte + nibble);
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     writeit     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
void writeit(void)
{
   unsigned char cksum,tmp;
            char oline[96];
   unsigned int  addr;
   unsigned char *ucptr;
            char *optr;
            int  count;

   addr = minaddr;
   ucptr = hex_array + addr;
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
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     hex_ascii   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
 *   Convert byte to two ASCII characters
*/
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
