/******************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1997-2004
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
*    Environment:  VME Data Acquisition System
*
*    File:         /tera/mcsq/Dlinux/Dacq/modu_setup.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    9/22/97    MCSQ
*
*    6/ 2/99    MCSQ        Add LeCroy 3377 TDC
*
*    9/ 1/99    MCSQ        Add shaper amplifier for microball
*
*    7/ 2/03    MCSQ        Add CAEN ADC and TDC modules(VME modules)
*
*   11//26/03   MCSQ        The CAEN TDC manual claims that the full range
*                           is linear from 140 to 1200 ns. IT IS NOT. 
*                           Paul Hausladen found that it is 1/x function.
*                           Changed this code to Hausladens formula for
*                           register value computation.
*****************************************************************************/
#include  "m_setup.h"
#include  <stdio.h>
#include  <string.h>

/*
*    Function prototypes
*/
void get_field(int,char *,int *,int *);
void error_msg(int ,int );
void mt_match(char *,int *,int *);
void dat_match(char *,int *,int *);

void read_phil(int *);
void read_silena(int *);
void read_lrs4300(int *);
void read_hhirf_adc(int *);
void read_lrs4508(int *);
void read_lrs4418(int *);
void read_ue_clock(int *);
void read_phil7106(int *);
void read_lrs4415a(int *);
void read_lrs3511(int *);
void read_lrs4413(int *);
void read_lrs4416b(int *);
void read_lrs3377(int *);
void read_shaper(int *);
void read_caen775(int *);
void read_caen785(int *);

void inhibit(char *);
void load_phil(int *);
void clear_silena(int *);
void load_silena(int *);
void load_hhirf_adc(int *);
void load_ue_clock(int *);
void load_lrs4508(int *);
void load_lrs4418(int *);
void load_lrs4300(int *);
void load_phil7106(int *);
void load_lrs4415a(int *);
void load_lrs3511(int *);
void load_lrs4413(int *);
void load_lrs4416b(int *);
void load_lrs3377(int *);
void load_shaper(int *);
void load_caen775(int *);
void load_caen785(int *);
/*
*    Global data
*/
      int line_num,mt_line,err_count;
      FILE *infile,*outfile;
      char line[256];
      int  indx[10][2];
      int  itype[10],nf,inext,cur;

/****************************************************************************
****************************************************************************/
int main(int argc, char *argv[])
{
      int  err,i,ln_num;
      int  mt_code,ftype,next;
      int  debug,mterr;
      char *cptr;
      char  mt[16];
      char  inname[80];
      char  outname[80];
      char  message[64];

      ln_num = 1;
infile = stdin;
      if (argc < 1)
       {
         printf(" Need filename for setup data file\n");
         exit(99);
       }
      strcpy(inname,argv[1]);
      infile = fopen(inname,"r");
      if (infile == NULL)
       {
         printf(" Can't open input file - %s\n",inname);
         exit(99);
       }

      strcpy(outname,inname);
      cptr = index(inname,'.');
      if (cptr != NULL)
       {
         *cptr = '\0';
       }
      strcat(outname,".log");
      debug = 0;
      if (argc == 2) debug = 1;
      mterr = 0;
      mt_line = 1;
      next = 1;
L10:  get_field(next,mt,&ftype,&err);
      next = 1;
      if (err == READ_EOF) goto L900;
      if (err == READ_MT) goto L14;
      mterr=ERR_READ_MT;
      goto L10;

L14:  if (mterr == 0)
       {
         error_msg(mterr,ln_num);
         err = 0;
       }
L15:  get_field(next,mt,&ftype,&err);
      if (err == READ_EOF) goto L900;
      if (err == READ_MT) goto L10;
      mt_line = line_num;
      printf(" $$ Line # %i:  Processing Module  %s\n",line_num,mt);
      mt_match(mt,&mt_code,&err);
/*******
      if (err == 0) goto L100;
      err = 0;
      if (mt_code == PHIL) read_phil(&err);
      else if (mt_code == SILENA) read_silena(&err);
      else if (mt_code == LRS_4300) read_lrs4300(&err);
      else if (mt_code == HHIRF_ADC) read_hhirf_adc(&err);
      else if (mt_code == LRS_4508) read_lrs4508(&err);
      else if (mt_code == LRS_4418) read_lrs4418(&err);
      else if (mt_code == UE_CLOCK) read_ue_clock(&err);
      else if (mt_code == PHIL_7106) read_phil7106(&err);
      else if (mt_code == LRS_4415A) read_lrs4415a(&err);
      else if (mt_code == LRS_3511) read_lrs3511(&err);
      else if (mt_code == LRS_4413) read_lrs4413(&err);
      else if (mt_code == LRS_4416B) read_lrs4416b(&err);
      else if (mt_code == LRS_3377) read_lrs3377(&err);
      else if (mt_code == SHAPER) read_shaper(&err);
      else if (mt_code == CAEN_775) read_caen775(&err);
      else if (mt_code == CAEN_785) read_caen785(&err);
L100: error_msg(err,ln_num);
      get_field(0,mt,&ftype,&err);
      if (err == READ_MT) goto L15;
      goto L10;
********/
L900:
/*******
      if (err_count == 0) exit(99);
      format_silena();
      if (debug != 0) 
       {
        outfile = fopen(outname,"w");
        if (outfile == NULL)
         {
           printf(" Can't open log file - %s\n",outname);
           exit(99);
         }
        list_modu();
       }
      err = 0;
      inhibit("save");
      load_phil(&err);
      clear_silena(&err);
      load_silena(&err);
      load_hhirf_adc(&err);
      load_ue_clock(&err);
      load_lrs4508(&err);
      load_lrs4418(&err);
      load_lrs4300(&err);
      load_phil7106(&err);
      load_lrs4415a(&err);
      load_lrs3511(&err);
      load_lrs4413(&err);
      load_lrs4416b(&err);
      load_lrs3377(&err);
      load_shaper(&err);
      load_caen775(&err);
      load_caen785(&err);
      inhibit("restore");
*******/
      if (err != 0) exit(99);
      exit(0);
}
/****************************************************************************
****************************************************************************/
void mt_match(char *mt,int *code,int *err)
{
       int i,j,len,match,slen,tlen;
       char *mt_types[23] = {"phil_7164","phil_7166","phil_7167",
        "phil_7186","phil_7187","lrs_4300","silena_4418","lrs_4508",
        "lrs_4418","hhirf_adc","ue_clock","phil_7106","lrs_4415a",
        "lrs_3511","lrs_4413","lrs_4416b","lrs_3351","lrs_3377",
        "shaper","caen_775","caen_785","mcsq",NULL};

static int mt_codes[23] = {PHIL,PHIL,PHIL,PHIL,PHIL,LRS_4300,
         SILENA,LRS_4508,LRS_4418,HHIRF_ADC,UE_CLOCK,PHIL_7106,
         LRS_4415A,LRS_3511,LRS_4413,LRS_4416B,SILENA,LRS_3377,
         SHAPER,CAEN_775,CAEN_785,MCSQ,0};

      *code = 0;
      *err = 0;
      len = strlen(mt);
      j = 0;
      match = 0;
      i = 0;
      while (mt_types[i] != NULL)
       {
        tlen = strlen(mt_types[i]);
        if (tlen < len) slen = tlen;
        else slen = len;
        if (!strncmp(mt,mt_types[i],slen))
         {
          j = j + 1;
          match = i;
         }
        i = i + 1;
       }
      if (j == 0) *err = ERR_MOD_UNKN;
      else if (j > 1) *err = ERR_MOD_AMBIG;
      else *code = mt_codes[match];
}
/****************************************************************************
****************************************************************************/
void dat_match(char *dat,int *code,int *err)
{

static char *dat_types[16] = {"lower_threshold","upper_threshold",
        "offset_memory","pedestals","common_threshold","enables",
        "ge_only","bgo_only","anticoinc","delays","mode","overflow",
        "conversion_gain","maximum_time","gain",NULL};
static int dat_codes[16] = {LOW,UP,OFF,PED,COM,ENABLE,
         GE_ONLY,BGO_ONLY,ANTI,DELAY,MODE,OVFL,MODE,UP,GAINS,0};

      int i,j,len,match,slen,tlen;

      *code = 0;
      *err = 0;
      len = strlen(dat);
      j = 0;
      match = 0;
      i = 1;
      while (dat_types[i] != NULL)
       {
        tlen = strlen(dat_types[i]);
        if (tlen < len) slen = tlen;
        else slen = len;
        if (!strncmp(dat,dat_types[i],slen))
         {
          j = j + 1;
          match = i;
         }
        i = i + 1;
       }
      if (j == 0) *err = ERR_DAT_UNKN;
      else if (j > 1) *err = ERR_DAT_AMBIG;
      else *code = dat_codes[match];
}
/******************************************************************************
*
*  Get command string
*
*  RETURNS:  modutype - 1 means module type in first field
*                       0 means data fields
*            endflag  - 1 means end of file
******************************************************************************/
void readcmd(int *modutype,int *endflag)
{
      int  i,j;

      *endflag = 0;
      *modutype = 0;
L5:
      if (inext == 0) 
       {
/*
*  Read new line
*/
L10:    if (fgets(line,sizeof(line)-1,infile) == NULL)
         {
           *endflag = 1;
           return;
         }
        line_num = line_num + 1;
        strlower_(line,sizeof(line));
        strparse_(line,indx,itype,&nf,&inext,sizeof(line));
/*
*  Ignore blank lines
*/
        if (nf == 0) goto L10;
        cur = 1;
        if (!strncmp(line+indx[0][0],"mt",indx[1][0]-indx[0][0]+1)) *modutype = 1;
       }
      else
       {
/*
*  Get additional fields from old line
*/
        i = inext;
        strparse_(line+i,indx,&itype,&nf,&inext,sizeof(line));
        cur = 1;
        if (inext != 0) inext = inext + i - 1;
        for(j=0; j < nf; j++)
         {
           indx[j][0] = indx[j][0] + i -1;
           indx[j][1] = indx[j][1] + i -1;
         }
       }
      for(j=0; j < nf; j++)
       {
/*
*  Comments begin with semicolon or asterisk
*/
        if (!(strncmp(line+indx[j][0],";",indx[j][1]-indx[j][0]+1)) ||
                    !(strncmp(line+indx[j][0],"*",indx[j][1]-indx[j][0]+1)))
         {
          inext = 0;
          if (j == 0) goto L5;
          nf = j -1;
          return;
         }
       }
      return;
}
