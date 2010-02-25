/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-2003
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
*    File:         /usr/users/mcsq/Dlinux/Dvme/acq_run.c
*
*    Description:  Workstation code to initialize, start and stop
*                  data acquisition.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/31/92    MCSQ        Original
*
*   10/30/92    MCSQ        Add acquisition status request function.
*
*   12/30/92    MCSQ        Added errors for non-existent hardware
*                           modules.  Changed to new Ethernet library.
*
*    1/ 9/93    MCSQ        Change for multiple VME systems.  The VME
*                           ethernet address is by default "vme".  If
*                           the enviroment variable VME is set, that
*                           string is used instead of "vme".
*                           Also added command line argument option.
*
*    3/29/93    MCSQ        Added code for non-existent KSC3892 list
*                           sequencer module.
*
*    4/23/94    MCSQ        Add command to get PAC name loaded in the
*                           VME processor.
*
*    2/23/95    MCSQ        Add error message for ACQ_SEQ_BUFFERS code.
*
*    2/27/96    MCSQ        Added error codes for new PAC features.
*                           i.e. count down, gates and special CAMAC
*                           modules.
*
*    8/29/96    MCSQ        Add command to get host Ethernet address
*
*   10/ 1/96    MCSQ        Add error code for illegal CAMAC module type
*                           in a Gate specification.
*
*    7/13/98    MCSQ        Add two new acquisition workstations to
*                           hostaddr[] array.
*
*    9/17/98    MCSQ        Add error codes for illegal crate number.
*
*    8/12/99    MCSQ        Add error codes for XIA electronics.
*
*    27/02/01   JWK         Add two Notre Dame workstations
*                           (Curie and Maxwell) to hostaddr[] array.
*
*    7/11/01    MCSQ        Add error codes for CAEN ADCs and TDCs.
*
*    9/28/01    MCSQ        Hostname and ethernet address data has been
*                           moved to the file /usr/acq/etc/acqwks.list.
*                           Added routine 'load_host_table' to read in
*                           this file when program starts.
*
*    7/27/02    MCSQ        Ported to Linux
*
*    3/19/03    MCSQ        Changed for new pkt_io.c
*
*    1/21/04    MCSQ        Add command to zero the acquisition VME clock
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"
#include "acq_ctl.h"

/*
*   Function Prototypes
*/
void ctl_error(char error);
void load_host_table(void);
#define HHIRF 1

/*
*   Global Variables
*/
char server[12] = "vme";
char *prog;
char line[81];
struct Vmed xbuf,rbuf;

char *cmd[] = {"init","start","stop","status","pacfile","host","zero",NULL};
char cmd_code[] = {INIT_ACQ,START_ACQ,STOP_ACQ,STATUS_ACQ,PAC_FILE,HOST,
                   ZERO_CLK,0};

/*
*  Host name and ethernet address table
*/
#define HW_ADDR_LEN 6
struct host_ether {
               char host[12];
      unsigned char addr[HW_ADDR_LEN];
}  hostaddr[50];

/****************************************************************************
****************************************************************************/
main(int argc, char *argv[])
{
   char *cptr;
   int i,status;
   char *outbuf, *inbuf;

   outbuf = xbuf.buf;
   inbuf = rbuf.buf;
   prog = argv[0];
   if ((cptr = getenv("VME")) != NULL) strcpy(server,cptr);

   load_host_table();
   while(1)
     {
       if (argc < 2)
         {
           printf("Command ? : ");
           if (fgets(line,80,stdin) == NULL) break;
           if (!strncmp(line,"end",3)) return(0);
           if (!strncmp(line,"exit",4)) return(0);
         }
       else   strcpy(line,argv[1]);
       for(i=0; i < 6; i++)
         {
           if(!strncmp(line,cmd[i],4)) break;
         }
       *outbuf = cmd_code[i];
       xbuf.len = 1;
       rbuf.len = 100;
       status = pkt_io(&xbuf,&rbuf,FECNTRL,5);
       if (status != 0)
         {
           ctl_error(status);
           exit(50);
         }
       status = 0;
       if (*inbuf != ACQ_OK)
         {
           ctl_error((char)*inbuf);
           status = *inbuf;
         }
       if (*outbuf == STATUS_ACQ)
         {
           switch (inbuf[1])
            {
              case  ACQ_RUN:
               printf("VME Acquisition is running\n");
               break;
              case  ACQ_STOP:
               printf("VME Acquisition is stopped\n");
               break;
              case  ACQ_UNINIT:
               printf("VME Acquisition has not been initialized\n");
               break;
              default:
               break;
            }
         }
       if (*outbuf == PAC_FILE)
         {
           if (inbuf[2] == 0) printf("No PAC file loaded.\n");
           else
             {
               printf("Loaded PAC file: %s\n",&inbuf[2]);
               if (inbuf[1] == ACQ_UNINIT)
                 {
                   printf("Loaded PAC has NOT been initialized.\n");
                 }
               else
                 {
                   printf("Loaded PAC has been initialized.\n");
                 }
             }
         }
       if (*outbuf == HOST)
         {
int j;

           i = 0;
/*************
for(j=1; j <7; j++) printf("%2.2x:",inbuf[j]);
printf("\n");
*************/
           while (hostaddr[i].host[0] != '\0')
            {
/*************
for(j=0; j <6; j++) printf("%2.2x:",hostaddr[i].addr[j]);
printf("\n");
*************/
              if (!memcmp(&inbuf[1],&hostaddr[i].addr,6))
                {
                  printf("Host workstation is %s\n",&hostaddr[i].host);
                  break;
                }
              i++;
            }
           if (hostaddr[i].host[0] == '\0') printf("Unknown host workstation!\n");
         }
       if (argc >= 2) break;
     }
   return(status);
}
/****************************************************************************
****************************************************************************/
void  ctl_error(char error)
{
   printf(" %s error \7 - ",prog);
   switch (error)
    {
      case  ACQ_UNKNOWN_COMMAND:
        printf("Unknown command\n");
        break;
      case  ACQ_INVALID_TABLE:
        printf("Invalid Acquisition table index\n");
        break;
      case  ACQ_STR_NOINIT:
        printf("Start failed because system not initialized!\n");
        break;
      case  ACQ_STR_RUN:
        printf("Acquisition already running\n");
        break;
      case  ACQ_STP_HALT:
        printf("Acquisition already stopped\n");
        break;
      case  ACQ_CAM_NOEXIST:
        printf("NonExistant CAMAC crate specified\n");
        break;
      case  ACQ_CAM_OFFLINE:
        printf("CAMAC crate(s) Off-Line\n");
        break;
      case  ACQ_CAM_INHIBIT:
        printf("Can not remove CAMAC INHIBIT\n");
        break;
      case  ACQ_CAM_INIT:
        printf("CAMAC timeout in Initialization list\n");
        break;
      case  ACQ_FB_NOEXIST:
        printf("FASTBUS module does not respond\n");
        break;
      case  ACQ_FB_UNKNOWN:
        printf("Unknown FASTBUS module type\n");
        break;
      case  ACQ_FERA_UNKNOWN:
        printf("Unknown FERA module type\n");
        break;
      case  ACQ_FERA_INIT:
        printf("CAMAC timeout error initializing a FERA module\n");
        break;
      case  ACQ_INIT_RUN:
        printf("Initialization Error - Acquisition is running\n");
        break;
      case  ACQ_NO_KSC2917:
        printf("No KSC2917 CAMAC interface found\n");
        break;
      case  ACQ_NO_LRS1131:
        printf("No LRS1131 VME interface to FASTBUS found\n");
        break;
      case  ACQ_NO_LRS1821:
        printf("No LRS1821 FASTBUS sequencer found\n");
        break;
      case  ACQ_NO_FERA:
        printf("No VME FERA readout module found\n");
        break;
      case  ACQ_NO_ACROMAG:
        printf("No ACROMAG trigger module found\n");
        break;
      case  ACQ_NO_TRIGGER:
        printf("No ORNL trigger module found\n");
        break;
      case  ACQ_NO_KSC3982:
        printf("No KSC3982 List Sequencer module found\n");
        break;
      case  ACQ_SEQ_BUFFERS:
        printf("List sequencer buffer in VMEacq is too small\n");
        break;
      case  ACQ_COUNT_DWN:
        printf("Count down list buffer in VMEacq is too small\n");
        break;
      case  ACQ_RAW_GATE:
        printf("Raw gate list buffer in VMEacq is too small\n");
        break;
      case  ACQ_CAL_GATE:
        printf("Calculated gate list buffer in VMEacq is too small\n");
        break;
      case  ACQ_CAM_RO:
        printf("CAMAC special module list buffer in VMEacq is too small\n");
        break;
      case  ACQ_CAM_UNKNOWN:
        printf("Unknown CAMAC module type\n");
        break;
      case  ACQ_GATE_MODTYPE:
        printf("Illegal CAMAC module type in Gate Spec.\n");
        break;
      case  ACQ_CR_COND:
        printf("Illegal crate in conditional read list.\n");
        break;
      case  ACQ_CR_UNCOND:
        printf("Illegal crate in unconditional read list.\n");
        break;
      case  ACQ_CR_LATCH:
        printf("Illegal crate in latch read list.\n");
        break;
      case  ACQ_CR_WIND:
        printf("Illegal crate in windup list.\n");
        break;
      case  ACQ_CR_GATE:
        printf("Illegal crate in gate read list.\n");
        break;
      case  ACQ_CR_CAMAC:
        printf("Illegal crate in special CAMAC module list.\n");
        break;
      case  ACQ_CR_RUN:
        printf("Illegal crate in $run list.\n");
        break;
      case  ACQ_NO_AUX:
        printf("No ORNL AUX controller found.\n");
        break;
      case  ACQ_NO_XIA:
        printf("No XIA modules found.\n");
        break;
      case  ACQ_NO_CAEN_ADC:
        printf("One or more CAEN ADC modules not found.\n");
        break;
      case  ACQ_NO_CAEN_TDC:
        printf("One or more CAEN TDC modules not found.\n");
        break;
      case  ACQ_NO_CAEN_QDC:
        printf("One or more CAEN QDC modules not found.\n");
        break;
      case  ETHER_RECEIVE:
        printf("Ethernet receive timeout.\n");
        break;
      case  ETHER_TRANSMIT:
        printf("Ethernet transmit error.\n");
        break;
      case  ETHER_OPEN:
        printf("Ethernet open failure.\n");
        break;
      default:
        printf("Unknown error code - %i\n",-error);
        break;
    }
}
/******************************************************************************
******************************************************************************/
/*
*   Load the list of acquisition hosts and ethernet addresses
*   The file format is expected to be:
*          #
*          #
*          #     comment lines
*          #
*          hostname  xx:xx:xx:xx:xx:xx
*             (as many lines as needed)
*/

void load_host_table(void)
{

#ifdef HHIRF
    char *hostfile="/usr/acq/etc/acqwks.list";
#else
    char *hostfile="/nfs_home/varner/Acq-development/hribf/mcsq/Dvme/acqwks.list";
#endif

    FILE *fp;
    int i,len;
    int num_of_nodes;
    char inps[80],*cptr;

    num_of_nodes = -1;
/*
*    Open the file, with precoded name
*/
    if ((fp=fopen(hostfile, "r")) == NULL)
     {
       printf("load_host_table: can't open %s\n",hostfile);
       exit(-1);
     }
    else
     {
/*
*     Read lines from the hosts file
*/
       while ((fgets(inps,sizeof(inps),fp)) != NULL)
        {

         if (inps[0] != '#')
          {
/*
*            get node name and ethernet address
*/

/************
printf("%s",inps);
************/

            cptr = strtok(inps," ");
            len = strlen(cptr);
            if (len > (sizeof(hostaddr->host)-1)) len = sizeof(hostaddr->host)-1;
            strncpy(hostaddr[++num_of_nodes].host,cptr,len);
            for (i=0; i< sizeof(hostaddr->addr); i++)
             {
               hostaddr[num_of_nodes].addr[i]=
                                              strtol(strtok(NULL,":"),NULL,16);
             }
            if (num_of_nodes >= (sizeof(hostaddr)/sizeof(struct host_ether) -1))
             {
               printf("load_host_table: too many hosts in file %s\n",hostfile);
               exit(-1);
             }
          }
        }
     }
    fclose(fp);
    return;
}
