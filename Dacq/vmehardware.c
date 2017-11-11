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
*    File:         /usr/users/mcsq/Dlinux/Dvme/vmehardware.c
*
*    Description:  List hardware configuration of the VME system.  Includes
*                  Ethernet addresses and available VME interface devices.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    5/ 5/93    MCSQ        Original
*
*    5/ 1/94    MCSQ        Add RMS control system VMIC 6015 modules
*
*    7/21/94    MCSQ        Add LeCroy 1190 Dual Port Memory module
*
*   10/ 2/94    MCSQ        Add temporary modules for RMS control
*
*   11/18/94    MCSQ        Change DATEL628 DAC to DATEL626 for RMS control
*
*   11/22/94    MCSQ        Add DATEL 613 ADC for RMS control
*
*    3/25/95    MCSQ        Add Green Spring IP-Stepper/IP-Digital 24
*
*    5/27/95    MCSQ        New modules for DRS control stystem.
*
*   10/16/95    MCSQ        Add DATEL 628 12-bit DAC
*
*    6/24/96    MCSQ        Add VMIC 3113A 12-bit ADC
*
*    2/17/97    MCSQ        Add three LRS1190 interfaces.  Add VMIC 3128
*                           64 channel 14-bit ADC for DRS.
*
*   12/13/97    MCSQ        Add a second KSC 2917 CAMAC interface
*
*   12/ 1/98    MCSQ        New VME interface to CAMAC
*
*    2/ 8/99    MCSQ        Add VMIC 3124  for LN fill system
*
*    4/ 4/01    MCSQ        Add 10 CAEN V785 ADCs
*
*    4/11/01    MCSQ        Add 10 CAEN V775 TDCs
*
*    7/26/02    MCSQ        Ported to Linux
*
*    3/19/03    MCSQ        Changed for new pkt_io.c
*
*    6/14/06    MCSQ        Add SIS3820 VME Scaler
*   04 June 2008 RLV        Added CAEN 785 and 775 numbers 11 and 12
*   27 July 2009 RLV        Added Catalin Matei work on CAEN V792
*****************************************************************************/
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <unistd.h>
#include  <errno.h>
#include "pkt_io_udp.h"
#include "mem_mgr.h"


/*
*    Function Prototypes
*/
void mgr_error(int );

/*
*    Global Variables
*/
static char server[12] = "vme";
char *prog;
extern int h_errno;              /* global host error number */
extern int errno;

/*   CAMAC data buffers             */
static struct UDP_Packet xbuf;

/****************************************************************************
*
*   List VME ethernet addresses and hardware devices found on boot.
****************************************************************************/
int main(int argc, char *argv[])
{
     char *cptr;
     //int i,size,status;
     int i,status;
     unsigned char *ucptr;
     struct Devices *cmd = (struct Devices *)xbuf.Data;
     struct Reply *rpy = (struct Reply *)xbuf.Data;

     prog = argv[0];
     if ((cptr = getenv("VME")) != NULL) strcpy(server,cptr);

     cmd->func = DEVICES;
     xbuf.DataSize = sizeof(struct Devices);
     status = pkt_io(&xbuf,&xbuf,CODE,1);
     if (status != 0)
       {
         mgr_error(status);
         exit(50);
       }
     if (rpy->status != OK)
       {
         mgr_error(rpy->status);
         exit(50);
       }
     printf("VME System Hardware Configuration\n\n");
     printf("    VME Processor Logical Name: %s\n",server);
     printf("VME Processor Ethernet Address: ");
     cmd = (struct Devices *)xbuf.Data;
     ucptr  = cmd->enet_vme;
     for (i=0; i < 5; i++) printf("%2.2x-",*ucptr++);
     printf("%2.2x\n",*ucptr++);
     printf(" Default Host Ethernet Address: ");
     ucptr  = cmd->enet_host;
     for (i=0; i < 5; i++) printf("%2.2x-",*ucptr++);
     printf("%2.2x\n",*ucptr++);
     printf("        Boot Multicast Address: ");
     ucptr  = cmd->enet_broad;
     for (i=0; i < 5; i++) printf("%2.2x-",*ucptr++);
     printf("%2.2x\n",*ucptr++);
     printf("Available Interface Modules are:\n\n");
     if (cmd->devtbl.ksc2917a != 0)
                             printf("KSC 2917A -  CAMAC Interface Module\n");
     if (cmd->devtbl.ksc2917b != 0)
                         printf("KSC 2917B -  Second CAMAC Interface Module\n");
     if (cmd->devtbl.lrs1131 != 0)
       {
         printf("LRS 1131  -  FASTBUS Interface Module\n");
         if (cmd->devtbl.lrs1821 != 0)
                           printf("          -  FASTBUS LRS 1821 SMI Module\n");
         else  printf("         - No LRS1821 SMI found\n");
       }
     if (cmd->devtbl.ces8170 != 0)
                             printf("CES 8170  -  FERA Readout Module\n");
     if (cmd->devtbl.trigger != 0)
                             printf("TRIGGER   -  ORNL Trigger Module\n");
     if (cmd->devtbl.acromag != 0)
                             printf("ACROMAG   -  Digital I/O Module\n");
     if (cmd->devtbl.vmic6015a != 0)
                             printf("VMIC6015A -  Quad-Serial I/O Module\n");
     if (cmd->devtbl.vmic6015b != 0)
                             printf("VMIC6015B -  Quad-Serial I/O Module\n");
     if (cmd->devtbl.lrs1190a != 0)
                            printf("LRS1190A  -  LeCroy FERA Readout Module\n");
     if (cmd->devtbl.lrs1190b != 0)
                            printf("LRS1190B  -  LeCroy FERA Readout Module\n");
     if (cmd->devtbl.lrs1190c != 0)
                            printf("LRS1190C  -  LeCroy FERA Readout Module\n");
     if (cmd->devtbl.lrs1190d != 0)
                            printf("LRS1190D  -  LeCroy FERA Readout Module\n");
     if (cmd->devtbl.datel626a != 0)
                            printf("DATEL626A -  Datel 16-bit DAC Module\n");
     if (cmd->devtbl.datel613a != 0)
                            printf("DATEL613A -  Datel 16-bit ADC Module\n");
     if (cmd->devtbl.datel613b != 0)
                            printf("DATEL613B -  Datel 16-bit ADC Module\n");
     if (cmd->devtbl.stepper != 0)
                            printf("STEPPER   -  Green Spring IP Module\n");
     if (cmd->devtbl.datel628a != 0)
                            printf("DATEL628A -  Datel 12-bit DAC Module\n");
     if (cmd->devtbl.vmic3113a != 0)
                            printf("VMIC3113A -  64 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.vmic3124 != 0)
                            printf("VMIC3124  -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.vmic3128a != 0)
                            printf("VMIC3128A -  64 Chan, 14-bit ADC Module\n");
     if (cmd->devtbl.ornlaux != 0)
                           printf("ORNLAUX   -  Fast VME interface to CAMAC\n");
     if (cmd->devtbl.caen785_1 != 0)
                           printf("CAEN785_1 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_2 != 0)
                           printf("CAEN785_2 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_3 != 0)
                           printf("CAEN785_3 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_4 != 0)
                           printf("CAEN785_4 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_5 != 0)
                           printf("CAEN785_5 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_6 != 0)
                           printf("CAEN785_6 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_7 != 0)
                           printf("CAEN785_7 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_8 != 0)
                           printf("CAEN785_8 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_9 != 0)
                           printf("CAEN785_9 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_10 != 0)
                           printf("CAEN785_10 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_11 != 0)
                           printf("CAEN785_11 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_12 != 0)
                           printf("CAEN785_12 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_13 != 0)
                           printf("CAEN785_13 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_14 != 0)
                           printf("CAEN785_14 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_15 != 0)
                           printf("CAEN785_15 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_16 != 0)
                           printf("CAEN785_16 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_17 != 0)
                           printf("CAEN785_17 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_18 != 0)
                           printf("CAEN785_18 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_19 != 0)
                           printf("CAEN785_19 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_20 != 0)
                           printf("CAEN785_20 -  32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_21 != 0)
                           printf("CAEN785_21 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_22 != 0)
                           printf("CAEN785_22 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_23 != 0)
                           printf("CAEN785_23 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen785_24 != 0)
                           printf("CAEN785_24 - 32 Chan, 12-bit ADC Module\n");
     if (cmd->devtbl.caen775_1 != 0)
                           printf("CAEN775_1 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_2 != 0)
                           printf("CAEN775_2 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_3 != 0)
                           printf("CAEN775_3 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_4 != 0)
                           printf("CAEN775_4 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_5 != 0)
                           printf("CAEN775_5 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_6 != 0)
                           printf("CAEN775_6 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_7 != 0)
                           printf("CAEN775_7 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_8 != 0)
                           printf("CAEN775_8 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_9 != 0)
                           printf("CAEN775_9 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_10 != 0)
                           printf("CAEN775_10 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_11 != 0)
                           printf("CAEN775_11 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen775_12 != 0)
                           printf("CAEN775_12 -  32 Chan, 12-bit TDC Module\n");
     if (cmd->devtbl.caen792_1 != 0)
                           printf("CAEN792_1 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_2 != 0)
                           printf("CAEN792_2 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_3 != 0)
                           printf("CAEN792_3 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_4 != 0)
                           printf("CAEN792_4 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_5 != 0)
                           printf("CAEN792_5 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_6 != 0)
                           printf("CAEN792_6 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_7 != 0)
                           printf("CAEN792_7 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_8 != 0)
                           printf("CAEN792_8 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_9 != 0)
                           printf("CAEN792_9 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_10 != 0)
                           printf("CAEN792_10 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_11 != 0)
                           printf("CAEN792_11 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.caen792_12 != 0)
                           printf("CAEN792_12 -  32 Chan, 12-bit QDC Module\n");
     if (cmd->devtbl.sis3820_1!= 0)
                           printf("SIS3820_1  -  32 Chan, VME Scaler Module\n");

     return (0);
}
/****************************************************************************
*
*  Report error code from the VME processor.
****************************************************************************/
void  mgr_error(int error)
{
   fprintf(stderr," %s error \7 - ",prog);
   switch (-error)
    {
      case  SRADRERR:
        fprintf(stderr,"S record address outside allocated memory\n");
        break;
      case  SRCHKSUM:
        fprintf(stderr,"S record checksum error\n");
        break;
      case  SRILLHEX:
        fprintf(stderr,"S record has nonhex character\n");
        break;
      case  SRCNTERR:
        fprintf(stderr,"S record byte count error\n");
        break;
      case  ILLINDEX:
        fprintf(stderr,"Illegal memory ID - must be in range 3 <= ID <= %d\n",MEM_SEGS);
        break;
      case  KILLERR:
        fprintf(stderr,"Request to kill nonexistent task\n");
        break;
      case  TSKMEM:
        fprintf(stderr,"Insufficient task memory allocated\n");
        break;
      case  ALLOCERR:
        fprintf(stderr,"Memory allocation table is full\n");
        break;
      case  ILLFUNC:
        fprintf(stderr,"Unrecognized command function code\n");
        break;
      case  PERMTSK:
        fprintf(stderr,"Request to kill a permamently resident task\n");
        break;
      case  NOMEMAVAIL:
        fprintf(stderr,"Not enough memory available\n");
        break;
      case  MEMDUPC:
        fprintf(stderr,"Duplicate memory/task name\n");
        break;
      case  ILLFREE:
        fprintf(stderr,"Free memory request error - size <= 0\n");
        break;
      case  -ETHER_RECEIVE:
        printf("Ethernet receive timeout.\n");
        break;
      case  -ETHER_TRANSMIT:
        printf("Ethernet transmit error.\n");
        break;
      case  -ETHER_OPEN:
        printf("Ethernet open failure.\n");
        break;
      default:
        fprintf(stderr,"Unknown error code - %i\n",-error);
        break;
    }
}
