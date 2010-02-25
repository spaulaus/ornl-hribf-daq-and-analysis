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
*
*    4/20/04    MCSQ        Converted Fortran code to C
*    30 July 2009 RLV       Adapt for 12 CAEN 775, 785 and 10 V792
*****************************************************************************/
#include  "m_setup.h"
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include "cam_fast.h"
#include  <math.h>
      

/*
*    Function prototypes
*/
void camacio_(int *,int *,int *,int *,int *,void *,int *, int *);
void camlist_(int *,int *,int *,int *,int *,void *,int *,int *);

void get_field(int,char *,int *,int *);
void get_numeric(int,int *,int *);
void error_msg(int ,int );
void mt_match(char *,int *,int *);
void dat_match(char *,int *,int *);
void get_slot(int *,int *,int *);
void dat_save(int ,int ,int ,struct modu_data ,int *);
void list_modu(void);
void inhibit(char *);
void format_silena(void);
void verify_error(int ,int ,int ,int ,unsigned short ,unsigned short );
void cam_error(int ,int ,int ,int ,int );
void m4508(int *,int *,char *);
void gen(int *,int *,int *);
void lanti(int *,int *);
void lbgo(int *,int *);
void lige(int *,int *);

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
void read_caen792(int *);

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
void load_caen792(int *);
/*
*    Global data
*/
      int line_num,mt_line,err_count;
      FILE *infile,*outfile;
      char line[256];
      int  indx[10][2];
      int  itype[10],nf,inext,cur;

      int cam_mode,cam_count;
      int cama,camf;
      int   cc[400],cn[400],ca[400],cf[400],st[400];
      unsigned short rd[400],wr[400];
/*
*   Module data
*/
      int m_index;
      struct modu_data modus[200];

/****************************************************************************
****************************************************************************/
int main(int argc, char *argv[])
{
      int  err,i,ln_num;
      int  mt_code,ftype,next;
      int  debug,mterr;
      char *cptr;
      char  mt[160];
      char  inname[80];
      char  outname[80];
      char  message[64];

      m_index = -1;
      ln_num = 1;
      if (argc < 2)
       {
         printf("Need filename for setup data file\n");
         exit(99);
       }
      strcpy(inname,argv[1]);
      infile = fopen(inname,"r");
      if (infile == NULL)
       {
         printf("Can't open input file - %s\n",inname);
         exit(99);
       }

      strcpy(outname,inname);
      cptr = strchr(outname,'.');
      if (cptr != NULL)
       {
         *cptr = '\0';
       }
      strcat(outname,".log");
      debug = 0;
      if (argc == 3) debug = 1;
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
      printf("$$ Line # %3i:  Processing Module  %s\n",line_num,mt);
      mt_match(mt,&mt_code,&err);
      if (err != 0) goto L100;
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
      else if (mt_code == CAEN_792) read_caen792(&err);
L100: error_msg(err,ln_num);
      get_field(0,mt,&ftype,&err);
      if (err == READ_MT) goto L15;
      goto L10;

L900:
      if (err_count != 0) exit(99);
      format_silena();
      if (debug != 0) 
       {
        outfile = fopen(outname,"w");
        if (outfile == NULL)
         {
           printf("Can't open log file - %s\n",outname);
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
      load_caen792(&err);
      inhibit("restore");
      if (err != 0) exit(99);
      exit(0);
}
/****************************************************************************
****************************************************************************/
void mt_match(char *mt,int *code,int *err)
{
  static       int i,j,len,match,slen,tlen;
  static char *mt_types[24] = {
    "phil_7164","phil_7166","phil_7167",
    "phil_7186","phil_7187","lrs_4300","silena_4418","lrs_4508",
    "lrs_4418","hhirf_adc","ue_clock","phil_7106","lrs_4415a",
    "lrs_3511","lrs_4413","lrs_4416b","lrs_3351","lrs_3377",
    "shaper","caen_775","caen_785","caen_792","mcsq",NULL};
  
  static int mt_codes[24] = {
    PHIL,PHIL,PHIL,PHIL,PHIL,LRS_4300,
    SILENA,LRS_4508,LRS_4418,HHIRF_ADC,UE_CLOCK,PHIL_7106,
    LRS_4415A,LRS_3511,LRS_4413,LRS_4416B,SILENA,LRS_3377,
    SHAPER,CAEN_775,CAEN_785,CAEN_792,MCSQ,0};
  
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
  
  static int i,j,len,match,slen,tlen;

  *code = 0;
  *err = 0;
  len = strlen(dat);
  j = 0;
  match = 0;
  i = 0;
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
  static int  i,j;
  
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
      strparse_(line,indx,itype,&nf,&inext,strlen(line));
      /*
       *  Ignore blank lines
       */
      if (nf == 0) goto L10;
      cur = 1;
      if (!strncmp(line+indx[0][0]-1,"mt",2)) *modutype = 1;
    }
  else
    {
      /*
       *  Get additional fields from old line
       */
      i = inext - 1;
      strparse_(line+i,indx,&itype,&nf,&inext,strlen(line)-i);
      cur = 1;
      if (inext != 0) inext = inext + i;
      for(j=0; j < nf; j++)
	{
	  indx[j][0] = indx[j][0] + i;
	  indx[j][1] = indx[j][1] + i;
	}
    }
  for(j=0; j < nf; j++)
    {
      /*
       *  Comments begin with semicolon or asterisk
       */
      if (!(strncmp(line+indx[j][0]-1,";",1)) ||
	  !(strncmp(line+indx[j][0]-1,"*",1)))
	{
          inext = 0;
          if (j == 0) goto L5;
          nf = j;
          return;
	}
    }
  return;
}
/******************************************************************************
 *
 *Call:    next  - int*4  0 means get current field again
 *                        nonzero means get next field
 *
 *Returns: field - character*(*)  ascii string
 *         ftype - int*4 - field type, 1 means alpha and 2 means numeric
 *         err   - 0 means normal field
 *                 READ_MT - module type field
 *                 READ_EOF - end-of-file
 ******************************************************************************/
void get_field(int next,char *field,int *ftype,int *err)
{
  static      int modutype,endflag;
  static      int hex,l,minus,temp;
  static      char string[160];
  
      *err = 0;
      if (next != 0) cur = cur + 1;
      if (cur != 0 && cur <= nf)
       {
        strncpy(field,line+indx[cur-1][0]-1,indx[cur-1][1]-indx[cur-1][0]+1);
        *ftype = itype[cur-1];
       }
      else
       {
        readcmd(&modutype,&endflag);
        strncpy(field,line+indx[cur-1][0]-1,indx[cur-1][1]-indx[cur-1][0]+1);
        *ftype = itype[cur-1];
       }
      field[indx[cur-1][1]-indx[cur-1][0]+1] = '\0';
      if (*ftype == 2)
       {
        hex = 0;
        minus = 0;
        strcpy(string,field);
        if (string[0] == '-')
         {
          strcpy(string,string+1);
          minus = 1;
         }
        l = strlen(string);
        if (l < 2)
         {
         }
        else if (string[0] == 'x' || string[0] == 'X')
         {
          strcpy(string,string+2);
          hex = 1;
         }
        else if (string[l-1] == 'h' || string[l-1] == 'H')
         {
          strncpy(string,string,l-1);
          hex = 1;
         }
        if (hex != 0)
         {
          sscanf(string,"%x",&temp);
          if (minus != 0) temp = -temp;
          sprintf(field,"%i",temp);
         }
      }
      if (cur == 1 && modutype != 0) *err = READ_MT;
      if (endflag != 0) *err = READ_EOF;
}
/******************************************************************************
*
*Call:    next  - int*4  0 means get current field again
*                        nonzero means get next field
*
*Returns: num   - int*4  number if numeric field
*         err   - int*4  ERR_NAN if alphanumeric field
*                        ERR_CONV if FORTRAN conversion error
******************************************************************************/
void get_numeric(int next,int *num,int *err)
{

static      int i,ftype;
static      char field[160];

      *err = 0;
      get_field(next,field,&ftype,err);
      if (*err != 0) return;
      if (ftype != 2)
       {
        *err = ERR_NAN;
        return;
       }
      if (sscanf(field,"%i",num) == 0)
       {
        *err = ERR_CONV;
       }
      return;
}
/******************************************************************************
*
*Call:    next  - int*4  0 means get current field again
*                        nonzero means get next field
*
*Returns: num   - int*4  number if numeric field
*         err   - int*4  ERR_NO_C if alphanumeric field
*                        ERR_CONV if FORTRAN conversion error
******************************************************************************/
void get_crate(int next,int *num,int *err)
{

static      int i,ftype;
static      char field[160];

      *err = 0;
      get_field(next,field,&ftype,err);
      if (*err != 0) return;
      if (ftype != 2)
       {
        *err = ERR_NO_C;
        return;
       }
      if (sscanf(field,"%i",num) == 0)
       {
        *err = ERR_CONV;
        return;
       }
      if (*num < 0 || *num > 17)
       {
        *err = ERR_C_VAL;
        return;
       }
      if (*num == 8 || *num == 9)
       {
        *err = ERR_C_VAL;
        return;
       }
      return;
}
/******************************************************************************
******************************************************************************/
void get_slot(int *n1,int *n2,int *err)
{

static      int i,j;
static      int ftype;
static      char field[160],*cptr;

      *err = 0;
      get_field(1,field,&ftype,err);
      if (ftype != 2)
       {
        *err = ERR_NO_N;
        return;
       }
      cptr = strchr(field,'-');
      if (cptr == NULL)
       {
        if (sscanf(field,"%i",n1) == 0) goto L10;
        *n2 = *n1;
       }
      else
       {
        *cptr++ = '\0';
        if (sscanf(field,"%i",n1) == 0) goto L10;
        if (sscanf(cptr,"%i",n2) == 0) goto L10;
       }
      if (*n1 < 1 || *n1 > 23)
       {
        *err = ERR_N_VAL;
        return;
       }
      if (*n2 < 1 || *n2 > 23)
       {
        *err = ERR_N_VAL;
        return;
       }
      if (*n2 < *n1)
       {
        *err = ERR_2N;
        return;
       }
      return;

L10:  *err = ERR_CONV;
      return;

}
/******************************************************************************
******************************************************************************/
void error_msg(int err,int ln_num)
{

static      int ln;
static      int val,ftype;
static      char field[160];

      if (err <= 0) return;
      err_count = err_count + 1;
      ln = line_num;
      get_field(0,field,&ftype,&val);
      if (val == READ_MT) ln = ln -1;
      if (mt_line != ln)
       {
        if (val != READ_MT)
         {
          printf("************** ERROR in lines %4.4i thru %4.4i **************\n\
    %s\n", mt_line,ln,line);
         }
        else
         {
          printf("************** ERROR in lines %4.4i thru %4.4i **************\n", mt_line,ln);
         }
       }
      else
       {
        printf("**** ERROR at line number %4.4i ****\n    %s\n",mt_line,line);
       }
      if (err == ERR_C_VAL)
       {
        printf("ERR: Invalid CAMAC crate number\n");
       }
      else if (err == ERR_N_VAL) 
        printf("ERR: Invalid CAMAC slot number\n");
      else if (err == ERR_2N) 
        printf("ERR: Second CAMAC slot number less than first\n");
      else if (err == ERR_MOD_UNKN) 
        printf("ERR: Unknown module type\n");
      else if (err == ERR_MOD_AMBIG) 
        printf("ERR: Ambiguous module type specification\n");
      else if (err == ERR_DAT_UNKN) 
        printf("ERR: Unknown data type\n");
      else if (err == ERR_DAT_AMBIG) 
        printf("ERR: Ambiguous data type specification\n");
      else if (err == ERR_CONV) 
        printf("ERR: Conversion error for numeric field\n");
      else if (err == ERR_NAN) 
        printf("ERR: Expected number, found alphanumeric field\n");
      else if (err == ERR_DATA_TYPE) 
        printf("ERR: Invalid data type for this module\n");
      else if (err == ERR_DATA_VAL) 
        printf("ERR: Data value out_of_range\n");
      else if (err == ERR_DUP_DATA) 
        printf("ERR: Duplicate data specified for this data type\n");
      else if (err == ERR_TOO_FEW) 
        printf("ERR: Insufficient data\n");
      else if (err == ERR_PRIOR_MOD_TYPE) 
        printf("ERR: Module type conflict.  See line # %4.4i\n",ln_num);
      else if (err == ERR_PRIOR_LOW) 
        printf("ERR: Lower Thresholds previously set at line %4.4i\n",ln_num);
      else if (err == ERR_PRIOR_UP) 
        printf("ERR: Upper Thresholds previously set at line %4.4i\n",ln_num);
      else if (err == ERR_PRIOR_OFF) 
        printf("ERR: Offset memory previously set at line %4.4i\n",ln_num);
      else if (err == ERR_PRIOR_PED) 
        printf("ERR: Pedestals previously set at line %4.4i\n",ln_num);
      else if (err == ERR_PRIOR_COM) 
        printf("ERR: Common threshold previously set at line %4.4i\n",ln_num);
      else if (err == ERR_PRIOR_ENABLE) 
        printf("ERR: Enables previously set at line %4.4i\n",ln_num);
      else if (err == ERR_PRIOR_GEN) 
        printf("ERR: Generate function previously set at line %4.4i\n",ln_num);
      else if (err == ERR_PRIOR_DELAY) 
        printf("ERR: Delays previously set at line %4.4i\n",ln_num);
      else if (err == ERR_MULTI_C) 
        printf("ERR: Multiple CAMAC Crate statements\n");
      else if (err == ERR_MULTI_N) 
        printf("ERR: Multiple CAMAC Slot statements\n");
      else if (err == ERR_NO_C) 
        printf("ERR: NO CAMAC Crate statement\n");
      else if (err == ERR_NO_N) 
        printf("ERR: NO CAMAC Slot statement\n");
      else if (err == ERR_TOO_MANY) 
        printf("ERR: Too many data\n");
      else if (err == ERR_UNKN_KEY) 
        printf("ERR: Unknown parameter\n");
      else if (err == ERR_DATA_ILL) 
        printf("ERR: Illegal data value\n");
      else if (err == ERR_READ_MT) 
        printf("ERR: No module type statement found - mt=\n");
      else
        printf("ERR: %4.4i Unknown error code");
      printf("\n");
}
/******************************************************************************
******************************************************************************/
void read_phil(int *err)
{

static int     i,j,k,code,strlen;
static int     ftype,num,c,n1,n2,next;
static int     dat_type[11],dat_end;
static int     dat_params[12][3] =
                {16, 0, 4095,     /* lower threshold   */
                 16, 0, 4095,     /* upper threshold   */
                  0, 0, 0,        /* offset memory     */
                 16, -4096, 4095, /* pedestals         */
                  0, 0, 0,        /* common threshold  */
                  0, 0, 0,        /* enable            */
                  0, 0, 0,        /* ge_only           */
                  0, 0, 0,        /* bgo_only          */
                  0, 0, 0,        /* anticoinc         */
                  0, 0, 0,        /* delay             */
                  0, 0, 0,        /* mode              */
                  0, 0, 0};       /* overflow          */
static int     dat_cnt,temp[32];
static char    field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.low_set = 0;
      temp_modu.up_set = 0;
      temp_modu.ped_set = 0;
      temp_modu.mt_type = PHIL;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
          *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
          *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
          *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
          *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for(i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
            *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
          *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if ((temp[i] < dat_params[k][1]) || (temp[i] > dat_params[k][2]))
             {
              *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
          if (k == LOW)
           {
            if (temp_modu.low_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.low[i] = temp[i];
            temp_modu.low_set = mt_line;
           }
          else if (k == UP)
           {
            if (temp_modu.up_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.up[i] = temp[i];
            temp_modu.up_set = mt_line;
           }
          else if (k == PED)
           {
            if (temp_modu.ped_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.ped[i] = temp[i];
            temp_modu.ped_set = mt_line;
           }
          else
            {
             if (ftype == 1) *err = ERR_UNKN_KEY;
             else *err = ERR_TOO_MANY;
             return;
            }
         }
       }
      goto L10;

L150:  *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_silena(int *err)
{
static  int   i,j,k,code;
static  int   ftype,num,c,n1,n2,next;
static  int   dat_type[11],dat_end;
static  int   dat_params[12][3] =
                 {8,    0, 1000,  /* lower threshold   */
                  8, 8500, 10000, /* upper threshold   */
                  8, -123,  123,  /* offset memory     */
                  0, 0, 0,        /* pedestals         */
                  1, 0, 1200,     /* common threshold  */
                  0, 0, 0,        /* enable            */
                  0, 0, 0,        /* ge_only           */
                  0, 0, 0,        /* bgo_only          */
                  0, 0, 0,        /* anticoinc         */
                  0, 0, 0,        /* delay             */
                  1, 0, 32767,    /* mode              */
                  1, 0, 1};       /* overflow          */

static  int  dat_cnt,temp[16];
static  char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.low_set = 0;
      temp_modu.up_set = 0;
      temp_modu.off_set = 0;
      temp_modu.com_set = 0;
      temp_modu.mode_set = 0;
      temp_modu.ovfl_set = 0;
      temp_modu.mt_type = SILENA;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
          *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
          *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
          *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
          *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err < 0) goto L150;
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
            *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
          *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err != 0) goto L150;
            if (temp[i] < dat_params[k][1] ||
                               temp[i] > dat_params[k][2])
             {
              *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
          if (k == LOW)
           {
            if (temp_modu.low_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.low[i] = temp[i];
            temp_modu.low_set = mt_line;
           }
          else if (k == UP)
           {
            if (temp_modu.up_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.up[i] = temp[i];
            temp_modu.up_set = mt_line;
           }
          else if (k == OFF)
           {
            if (temp_modu.off_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.off[i] = temp[i];
            temp_modu.off_set = mt_line;
           }
          else if (k == COM)
           {
            if (temp_modu.com_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.com = temp[0];
            temp_modu.com_set = mt_line;
           }
          else if (k == MODE)
           {
            if (temp_modu.mode_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.mode = temp[0];
            temp_modu.mode_set = mt_line;
           }
          else if (k == OVFL)
           {
            if (temp_modu.ovfl_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.ovfl = temp[0];
            temp_modu.ovfl_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_lrs4300(int *err)
{

static  int i,j,k,code;
static  int ftype,num,c,n1,n2,next;
static  int dat_type[11],dat_end;
static  int  dat_params[12][3] =
                 { 0, 0, 0,     /* lower threshold   */
                   0, 0, 0,     /* upper threshold   */
                   0, 0, 0,     /* offset memory     */
                  16, 0, 255,   /* pedestals         */
                   0, 0, 0,     /* common threshold  */
                   0, 0, 0,     /* enable            */
                   0, 0, 0,     /* ge_only           */
                   0, 0, 0,     /* bgo_only          */
                   0, 0, 0,     /* anticoinc         */
                   0, 0, 0,     /* delay             */
                   1, 0, 65535, /* mode              */
                   1, 0, 1};    /* overflow          */
static  int  dat_cnt,temp[16];
static  char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.ped_set = 0;
      temp_modu.mode_set = 0;
      temp_modu.ovfl_set = 0;
      temp_modu.mt_type = LRS_4300;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
          *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
          *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
          *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
          *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
            *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
          *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
              *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
          if (k == PED)
           {
            if (temp_modu.ped_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.ped[i] = temp[i];
            temp_modu.ped_set = mt_line;
           }
          else if (k == MODE)
           {
            if (temp_modu.mode_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.mode = temp[0];
            temp_modu.mode_set = mt_line;
           }
          else if (k == OVFL)
           {
            if (temp_modu.ovfl_set != 0)
             {
              *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.ovfl = temp[0];
            temp_modu.ovfl_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_hhirf_adc(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             0, 0, 0,     /* offset memory */
             0, 0, 0,     /* pedestals */
             0, 0, 0,     /* common threshold */
            16, 0, 1,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             0, 0, 0,     /* mode */
             0, 0, 0};    /* overflow */
static int  dat_cnt,temp[16];
static char field[160];
static struct modu_data temp_modu;

/**************
             int ii,kk;

             for(ii=0; ii < 12; ii++)
              {
                printf("%2i ",ii);
                for (kk=0; kk < 3; kk++) printf("%7i",dat_params[ii][kk]);
                printf("\n");
              }
**************/

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.enable_set = 0;
      temp_modu.mt_type = HHIRF_ADC;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
          *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
          *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
          *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {      
        if (n1 > 0)
         {
          *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
            *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
          *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
printf("hhirf: %i   %i %i\n",temp[i],dat_params[k][1],dat_params[k][2]);
              *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
          if (temp_modu.enable_set != 0)
           {
            *err = ERR_DUP_DATA;
            return;
           }
          for (i=0; i < dat_params[k][0]; i++) temp_modu.enable[i] = temp[i];
          temp_modu.enable_set = mt_line;
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_lrs4418(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             0, 0, 0,     /* offset memory */
             0, 0, 0,     /* pedestals */
             0, 0, 0,     /* common threshold */
             0, 0, 0,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
            16, 0, 128,   /* delay */
             0, 0, 0,     /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[16];
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.delay_set = 0;
      temp_modu.mt_type = LRS_4418;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
          if (temp_modu.delay_set != 0)
           {
           *err = ERR_DUP_DATA;
            return;
           }
          for (i=0; i < dat_params[k][0]; i++) temp_modu.delay[i] = temp[i];
          temp_modu.delay_set = mt_line;
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_lrs4508(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             0, 0, 0,     /* offset memory */
             0, 0, 0,     /* pedestals */
             0, 0, 0,     /* common threshold */
             0, 0, 0,     /* enable */
             1, 0, 0,     /* ge_only */
             1, 0, 0,     /* bgo_only */
             1, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             0, 0, 0,     /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[16];
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.gen_set = 0;
      temp_modu.mt_type = LRS_4508;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err > 0) return;
          if (*err < 0) goto L100;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        if (dat_end != 1)
         {
         *err = ERR_DUP_DATA;
          return;
         }
        if (temp_modu.gen_set == 0)
         {
          temp_modu.gen_type = dat_type[0];
          temp_modu.gen_set = mt_line;
         }
        else
         {
         *err = ERR_DUP_DATA;
          return;
         }
        next = 0;
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

}
/******************************************************************************
******************************************************************************/
void read_ue_clock(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             0, 0, 0,     /* offset memory */
             0, 0, 0,     /* pedestals */
             0, 0, 0,     /* common threshold */
             0, 0, 0,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             1, 0, 1,     /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[16];
static char  mode0[16] = {"mode_0"};
static char  mode1[16] = {"mode_1"};
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.mode_set = 0;
      temp_modu.mt_type = UE_CLOCK;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          if (!strcmp(field,mode0))  goto L100;
          if (!strcmp(field,mode1)) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          if (k == MODE)
           {
            get_field(next,field,&ftype,err);
            if (*err != 0) goto L150;
            if (ftype == 1)
             {
              temp[0] = -1;
              if (!strcmp(field,mode0)) temp[0] = 0;
              if (!strcmp(field,mode1)) temp[0] = 1;
              next = 1;
              if (temp[0] >= 0) goto L110;
             }
            next = 0;
           }
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
L110:     if (k == MODE)
           {
            if (temp_modu.mode_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.mode = temp[0];
            temp_modu.mode_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_phil7106(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             0, 0, 0,     /* offset memory */
             0, 0, 0,     /* pedestals */
             1, 10, 1033, /* common threshold */
            16, 0, 1,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             0, 0, 0,     /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[16];
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.com_set = 0;
      temp_modu.enable_set = 0;
      temp_modu.mt_type = PHIL_7106;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            temp[i] = abs(temp[i]);
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
          if (k == COM)
           {
            if (temp_modu.com_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.com = temp[i] - 10;
            temp_modu.com_set = mt_line;
           }
          else if (k == ENABLE)
           {
            if (temp_modu.enable_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.enable[i] = temp[i];
            temp_modu.enable_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_lrs4415a(int *err)
{

static int  i,j,k,code,len;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             0, 0, 0,     /* offset memory */
             0, 0, 0,     /* pedestals */
             0, 0, 0,     /* common threshold */
            16, 0, 1,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             1, 0, 1,     /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[16];
static char field[160];
static char test[32] = {"test"};
static char normal[32] = {"normal"};
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.enable_set = 0;
      temp_modu.mode_set = 0;
      temp_modu.mt_type = LRS_4415A;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          len = strlen(field);
          if (!strncmp(field,test,len))  goto L100;
          if (!strncmp(field,normal,len))  goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          if (k == MODE)
           {
            get_field(next,field,&ftype,err);
            if (*err != 0) goto L150;
            if (ftype == 1)
             {
              len = strlen(field);
              temp[0] = -1;
              if (!strncmp(field,test,len)) temp[0] = 1;
              if (!strncmp(field,normal,len)) temp[0] = 0;
              next = 1;
              if (temp[0] >= 0) goto L110;
             }
            next = 0;
           }
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
L110:     if (k == ENABLE)
           {
            if (temp_modu.enable_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.enable[i] = temp[i];
            temp_modu.enable_set = mt_line;
           }
          else if (k == MODE)
           {
            if (temp_modu.mode_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.mode = temp[0];
            temp_modu.mode_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_lrs3511(int *err)
{

static int  i,j,k,code,len;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             1, 0, 65535, /* offset memory */
             0, 0, 0,     /* pedestals */
             0, 0, 0,     /* common threshold */
             0, 0, 0,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             1, 250, 8000, /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[16];
static int  cg[6] =  {8000,4000,2000,1000,500,250};
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.off_set = 0;
      temp_modu.mode_set = 0;
      temp_modu.mt_type = LRS_3511;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          len = strlen(field);
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
L110:     if (k == OFF)
           {
            if (temp_modu.off_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.off[0] = temp[1];
            temp_modu.off_set = mt_line;
           }
          else if (k == MODE)
           {
            if (temp_modu.mode_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < 6; i++) if (cg[i] == temp[0]) goto L130;
           *err = ERR_DATA_ILL;
            return;

L130:       temp_modu.mode = (i+1)*256;
            temp_modu.mode_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_lrs4413(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             0, 0, 0,     /* offset memory */
             0, 0, 0,     /* pedestals */
             1, 0, 1023,  /* common threshold */
            16, 0, 1,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             0, 0, 0,     /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[16];
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.com_set = 0;
      temp_modu.enable_set = 0;
      temp_modu.mt_type = LRS_4413;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            temp[i] = abs(temp[i]);
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
          if (k == COM)
           {
            if (temp_modu.com_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.com = temp[i];
            temp_modu.com_set = mt_line;
           }
          else if (k == ENABLE)
           {
            if (temp_modu.enable_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.enable[i] = temp[i];
            temp_modu.enable_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_lrs4416b(int *err)
{

static int  i,j,k,code,len;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             0, 0, 0,     /* offset memory */
             0, 0, 0,     /* pedestals */
             1, 0, 1023,  /* common threshold */
            16, 0, 1,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             0, 0, 0,     /* mode */
             0, 0, 0};    /* overflow */
static int  dat_cnt,temp[16];
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.enable_set = 0;
      temp_modu.com_set = 0;
      temp_modu.mt_type = LRS_4416B;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            temp[i] = abs(temp[i]);
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
L110:     if (k == ENABLE)
           {
            if (temp_modu.enable_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.enable[i] = temp[i];
            temp_modu.enable_set = mt_line;
           }
          else if (k == COM)
           {
            if (temp_modu.com_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.com = temp[0];
            temp_modu.com_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_lrs3377(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             1, 0, 32767, /* full scale time */
             0, 0, 0,     /* offset memory */
             0, 0, 0,     /* pedestals */
             1, 0, 1,     /* common mode (stop/start) */
             0, 0, 0,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             0, 0, 0,     /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[16];
static char start[16] = {"start"};
static char stop[16] = {"stop"};
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.up_set = 0;
      temp_modu.com_set = 0;
      temp_modu.mt_type = LRS_3377;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          if (!strcmp(field,start))  goto L100;
          if (!strcmp(field,stop)) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          if (k == COM)
           {
            get_field(next,field,&ftype,err);
            if (*err != 0) goto L150;
            if (ftype == 1)
             {
              temp[0] = -1;
              if (!strcmp(field,start)) temp[0] = 0;
              if (!strcmp(field,stop)) temp[0] = 1;
              next = 1;
              if (temp[0] >= 0) goto L110;
             }
            next = 0;
           }
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
L110:     if (k == COM)
           {
            if (temp_modu.com_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.com = temp[0];
            temp_modu.com_set = mt_line;
           }
          else if (k == UP)
           {
            if (temp_modu.up_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.up[i] = temp[i];
            temp_modu.up_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_shaper(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 0, 0, 0,     /* lower threshold */
             0, 0, 0,     /* upper threshold */
             0, 0, 0,     /* offset memory */
            16, 0, 255,   /* pedestals/gains */
             0, 0, 0,     /* common threshold */
             0, 0, 0,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             0, 0, 0,     /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[16];
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.ped_set = 0;
      temp_modu.mode_set = 0;
      temp_modu.ovfl_set = 0;
      temp_modu.mt_type = SHAPER;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (c < 0)
         {
         *err = ERR_NO_C;
          return;
         }
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   CAMAC Crate number ?
*/
      if (!strcmp(field,"c") || !strcmp(field,"crate"))
       {
        if (c >= 0)
         {
         *err = ERR_MULTI_C;
          return;
         }
        get_crate(next,&c,err);
        if (*err != 0) return;
       }
/*
*   CAMAC Slot numbers ?
*/
      else if (!strcmp(field,"n") || !strcmp(field,"slot"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
          if (k == GAINS)
           {
            if (temp_modu.ped_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.ped[i] = temp[i];
            temp_modu.ped_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}

/******************************************************************************
******************************************************************************/
void read_caen775(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
  { 32,  0, 2047, /* lower threshold */
    1, 140, 1200, /* full scale time */
    0, 0, 0,     /* offset memory */
    0, 0, 0,     /* pedestals */
    1, 0, 1,     /* common mode (stop/start) */
    0, 0, 0,     /* enable */
    0, 0, 0,     /* ge_only */
    0, 0, 0,     /* bgo_only */
    0, 0, 0,     /* anticoinc */
    0, 0, 0,     /* delay */
    0, 0, 0,     /* mode */
    0, 0, 0};    /* overflow */
static int  dat_cnt,temp[32];
static char start[16] = "start";
static char stop[16] = "stop";
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.low_set = 0;
      temp_modu.up_set = 0;
      temp_modu.com_set = 0;
      temp_modu.mt_type = CAEN_775;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   TDC numbers ?
*/
      if (!strcmp(field,"n") || !strcmp(field,"tdc"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
      }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          if (!strcmp(field,start))  goto L100;
          if (!strcmp(field,stop)) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          if (k == COM)
           {
            get_field(next,field,&ftype,err);
            if (*err != 0) goto L150;
            if (ftype == 1)
             {
              temp[0] = -1;
              if (!strcmp(field,start)) temp[0] = 0;
              if (!strcmp(field,stop)) temp[0] = 1;
              next = 1;
              if (temp[0] >= 0) goto L110;
             }
            next = 0;
           }
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
L110:     if (k == COM)
           {
            if (temp_modu.com_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            temp_modu.com = temp[0];
            temp_modu.com_set = mt_line;
           }
          else if (k == LOW)
           {
            if (temp_modu.low_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.low[i] = temp[i];
            temp_modu.low_set = mt_line;
           }
          else if (k == UP)
           {
            if (temp_modu.up_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++)
             {
              temp_modu.up[i] = temp[i];
             }
            temp_modu.up_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}

/******************************************************************************
******************************************************************************/
void read_caen785(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
  { 32,   0, 2047, /* lower threshold */
    0, 0, 0,       /* upper threshold */
    0, 0, 0,     /* offset memory */
    0, 0, 0,     /* pedestals */
    0, 0, 0,     /* common mode */
    0, 0, 0,     /* enable */
    0, 0, 0,     /* ge_only */
    0, 0, 0,     /* bgo_only */
    0, 0, 0,     /* anticoinc */
    0, 0, 0,     /* delay */
    0, 0, 0,     /* mode */
    0, 0, 0};    /* overflow */
static int  dat_cnt,temp[32];
static char field[160];
static struct modu_data temp_modu;

      c = -1;
      n1 = -1;
      n2 = 0;
      temp_modu.low_set = 0;
      temp_modu.mt_type = CAEN_785;
      temp_modu.mt_line = mt_line;
      next = 1;
L10:  get_field(next,field,&ftype,err);
      if (*err != 0)
       {
        if (n1 < 0)
         {
         *err = ERR_NO_N;
          return;
         }
        dat_save(c,n1,n2,temp_modu,err);
        return;
       }
/*
*   ADC numbers ?
*/
      if (!strcmp(field,"n") || !strcmp(field,"adc"))
       {
        if (n1 > 0)
         {
         *err = ERR_MULTI_N;
          return;
         }
        get_slot(&n1,&n2,err);
        if (*err != 0) return;
       }
/*
*   Data specification
*/
      else if (!strcmp(field,"data"))
       {
        for (i=0; i < 11; i++)
         {
          get_field(next,field,&ftype,err);
          if (*err != 0) goto L150;
          if (ftype == 2) goto L100;
          dat_match(field,&code,err);
          if (*err != 0) return;
          if (dat_params[code][0] <= 0)
           {
           *err = ERR_DATA_TYPE;
            return;
           }
          dat_type[i] = code;
         }
L100:
        dat_end = i;
        if (dat_end == 0)
         {
         *err = ERR_DATA_TYPE;
          return;
         }
        next = 0;
        for (j=0; j < dat_end; j++)
         {
          k = dat_type[j];
          for (i=0; i < dat_params[k][0]; i++)
           {
            get_numeric(next,&temp[i],err);
            if (*err < 0) goto L150;
            if (*err != 0) return;
            if (temp[i] < dat_params[k][1] || temp[i] > dat_params[k][2])
             {
             *err = ERR_DATA_VAL;
              return;
             }
            next = 1;
           }
L110:     if (k == LOW)
           {
            if (temp_modu.low_set != 0)
             {
             *err = ERR_DUP_DATA;
              return;
             }
            for (i=0; i < dat_params[k][0]; i++) temp_modu.low[i] = temp[i];
            temp_modu.low_set = mt_line;
           }
         }
       }
      else
       {
        if (ftype == 1) *err = ERR_UNKN_KEY;
        else *err = ERR_TOO_MANY;
        return;
       }
      goto L10;

L150: *err = ERR_TOO_FEW;
      return;
}
/******************************************************************************
******************************************************************************/
void read_caen792(int *err)
{

static int  i,j,k,code;
static int  ftype,num,c,n1,n2,next;
static int  dat_type[11],dat_end;
static int  dat_params[12][3] =
           { 32, 0, 2047, /* lower threshold */
             0, 0, 0,     /* upper threshold (not used) */
             0, 0, 0,     /* offset memory */
             1, 45, 255,  /* pedestals register*/
             0, 0, 0,     /* common mode (stop/start) */
             0, 0, 0,     /* enable */
             0, 0, 0,     /* ge_only */
             0, 0, 0,     /* bgo_only */
             0, 0, 0,     /* anticoinc */
             0, 0, 0,     /* delay */
             0, 0, 0,     /* mode */
             0, 0, };    /* overflow */
static int  dat_cnt,temp[32];
static char field[160];
static struct modu_data temp_modu;

 c = -1;
 n1 = -1;
 n2 = 0;
 temp_modu.low_set = 0;
 temp_modu.up_set = 0;
 temp_modu.mt_type = CAEN_792;
 temp_modu.mt_line = mt_line;
 next = 1;
 while (1) {
   get_field(next,field,&ftype,err);
   if (*err != 0) {
     if (n1 < 0) {
       *err = ERR_NO_N;
       return;
     }
     dat_save(c,n1,n2,temp_modu,err);
     return;
   }
   /*
    *   QDC numbers ?
    */
   if (!strcmp(field,"n") || !strcmp(field,"qdc")) {
     if (n1 > 0) {
       *err = ERR_MULTI_N;
       return;
     }
     get_slot(&n1,&n2,err);
     if (*err != 0) return;
   }
   /*
    *   Data specification
    */
   else if (!strcmp(field,"data")) {
     for (i=0; i < 11; i++) {
       get_field(next,field,&ftype,err);
       if (*err != 0) {
	 *err = ERR_TOO_FEW;
	 return;
       }
       if (ftype == 2) break;  //Number means stop looking for strings
       dat_match(field,&code,err);
       if (*err != 0) return;
       if (dat_params[code][0] <= 0) {
	 *err = ERR_DATA_TYPE;
	 return;
       }
       dat_type[i] = code;
     }
     dat_end = i;
     if (dat_end == 0) {
       *err = ERR_DATA_TYPE;
       return;
     }
     next = 0;
     for (j=0; j < dat_end; j++) {
       k = dat_type[j];
       for (i=0; i < dat_params[k][0]; i++) {
	 get_numeric(next,&temp[i],err);
	 if (*err < 0) {
           printf("v792 - getting data\n");
	   *err = ERR_TOO_FEW;
	   return;
	 }
	 if (*err != 0) return;
	 if (temp[i] < dat_params[k][1] || 
	     temp[i] > dat_params[k][2]) {
	   *err = ERR_DATA_VAL;
	   return;
	 }
	 next = 1;
       }
       L110: 
       if (k == LOW) {
	 if (temp_modu.low_set != 0) {
	   *err = ERR_DUP_DATA;
	   return;
	 }
	 for (i=0; i < dat_params[k][0]; i++) 
	   temp_modu.low[i] = temp[i];
	 temp_modu.low_set = mt_line;
       }
       else if (k == PED) {
	 if (temp_modu.ped_set != 0) {
	   *err = ERR_DUP_DATA;
	   return;
	 }
	 for (i=0; i < dat_params[k][0]; i++)
	   temp_modu.ped[i] = temp[i];

	 temp_modu.ped_set = mt_line;
       }
     }
   }
   else {
     if (ftype == 1) 
       *err = ERR_UNKN_KEY;
     else 
       *err = ERR_TOO_MANY;
     return;
   }
 }
 
 L150: *err = ERR_TOO_FEW;
 return;
}
/******************************************************************************
******************************************************************************/
void dat_save(int c,int n1,int n2,struct modu_data temp_modu,int *err)
{
static      int i,j,k,new;

      for (i=n1; i <= n2; i++)
       {
        new = -1;
        for (j=0; j <= m_index; j++)
         {
          if (c == -1)
           {
            if (modus[j].n == i && modus[j].mt_type == temp_modu.mt_type)
             {
              new = j;
              goto L10;
             }
           }
          else
           {
            if (modus[j].c == c && modus[j].n == i)
             {
              new = j;
              goto L10;
             }
           }
         }
L10:    if (new < 0)
         {
          m_index = m_index + 1;
          modus[m_index] = temp_modu;
          modus[m_index].c = c;
          modus[m_index].n = i;
         }
        else
         {
          if (temp_modu.mt_type != modus[new].mt_type)
           {
            error_msg(ERR_PRIOR_MOD_TYPE,modus[new].mt_line);
            return;
           }
          if (temp_modu.low_set != 0)
           {
            if (modus[new].low_set != 0)
             {
              error_msg(ERR_PRIOR_LOW,modus[new].low_set);
              return;
             }
            for (k=0; k < 16; k++) modus[new].low[k] = temp_modu.low[k];
            modus[new].low_set = temp_modu.low_set;
           }
          if (temp_modu.up_set != 0)
           {
            if (modus[new].up_set != 0)
             {
              error_msg(ERR_PRIOR_UP,modus[new].up_set);
              return;
             }
            for (k=0; k < 16; k++) modus[new].up[k] = temp_modu.up[k];
            modus[new].up_set = temp_modu.up_set;
           }
          if (temp_modu.ped_set == 0)
           {
            if (modus[new].ped_set != 0)
             {
              error_msg(ERR_PRIOR_PED,modus[new].ped_set);
              return;
             }
            for (k=0; k < 16; k++) modus[new].ped[k] = temp_modu.ped[k];
            modus[new].ped_set = temp_modu.ped_set;
           }
          if (temp_modu.off_set != 0)
           {
            if (modus[new].off_set != 0)
             {
              error_msg(ERR_PRIOR_OFF,modus[new].off_set);
              return;
             }
            for (k=0; k < 16; k++) modus[new].off[k] = temp_modu.off[k];
            modus[new].off_set = temp_modu.off_set;
           }
          if (temp_modu.com_set != 0)
           {
            if (modus[new].com_set != 0)
             {
              error_msg(ERR_PRIOR_COM,modus[new].com_set);
              return;
             }
            modus[new].com = temp_modu.com;
            modus[new].com_set = temp_modu.com_set;
           }
          if (temp_modu.enable_set != 0)
           {
            if (modus[new].enable_set != 0)
             {
              error_msg(ERR_PRIOR_ENABLE,modus[new].enable_set);
              return;
             }
            for (k=0; k < 16; k++) modus[new].enable[k] = temp_modu.enable[k];
            modus[new].enable_set = temp_modu.enable_set;
           }
          if (temp_modu.gen_set != 0)
           {
            if (modus[new].gen_set != 0)
             {
              error_msg(ERR_PRIOR_GEN,modus[new].gen_set);
              return;
             }
            modus[new].gen_type = temp_modu.gen_type;
            modus[new].gen_set = temp_modu.gen_set;
           }
          if (temp_modu.delay_set != 0)
           {
            if (modus[new].delay_set != 0)
             {
              error_msg(ERR_PRIOR_DELAY,modus[new].delay_set);
              return;
             }
            for (k=0; k < 16; k++) modus[new].delay[k] = temp_modu.delay[k];
            modus[new].delay_set = temp_modu.delay_set;
           }
         }
       }
      return;
}
/******************************************************************************
******************************************************************************/
void list_modu(void)
{
static      int i,j,k;
static      char  *mtype[17]= {"phillips","silena","LRS_4300",
        "HHIRF_ADC","LRS_4508","LRS_4418","ue_clock",
        "phil_7106","LRS_4415A","LRS_3511","LRS_4413",
        "LRS_4416B","LRS_3377","SHAPER","CAEN_775","CAEN_785",
        "MCSQ"};

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        fprintf(outfile,"************ module # %3i\n",i+1);
        j = modus[i].mt_type;
        fprintf(outfile,"%12i  module type  %s\n",modus[i].mt_type,mtype[j-1]);
        fprintf(outfile,"%12i  module line #\n",modus[i].mt_line);
        fprintf(outfile,"%12i  module error\n",modus[i].mt_error);
        fprintf(outfile,"%12i  module crate #\n",modus[i].c);
        fprintf(outfile,"%12i  module slot #\n",modus[i].n);
        if (modus[i].low_set != 0)
         {
          fprintf(outfile,"%12i  low line # and values\n",modus[i].low_set);
          k = 0;
          for (j=0; j < sizeof(modus[i].low)/sizeof(int); j++)
           {
             fprintf(outfile,"%6i",modus[i].low[j]);
             k++;
             if ((k % 8) == 0) fprintf(outfile,"\n");
           }
         }
        if (modus[i].up_set != 0)
         {
          fprintf(outfile,"%12i  upper line # and values\n",modus[i].up_set);
          k = 0;
          for (j=0; j < sizeof(modus[i].up)/sizeof(int); j++)
           {
             fprintf(outfile,"%6i",modus[i].up[j]);
             k++;
             if ((k % 8) == 0) fprintf(outfile,"\n");
           }
         }
        if (modus[i].ped_set != 0)
         {
          fprintf(outfile,"%12i  peds line # and values\n",modus[i].ped_set);
          k = 0;
          for (j=0; j < sizeof(modus[i].ped)/sizeof(int); j++)
           {
             fprintf(outfile,"%6i",modus[i].ped[j]);
             k++;
             if ((k % 8) == 0) fprintf(outfile,"\n");
           }
         }
        if (modus[i].off_set != 0)
         {
          fprintf(outfile,"%12i  offset line # and values\n",modus[i].off_set);
          k = 0;
          for (j=0; j < sizeof(modus[i].off)/sizeof(int); j++)
           {
             fprintf(outfile,"%6i",modus[i].off[j]);
             k++;
             if ((k % 8) == 0) fprintf(outfile,"\n");
           }
         }
        if (modus[i].com_set != 0)
         {
          fprintf(outfile,"%12i  common line # and value\n",modus[i].com_set);
          fprintf(outfile,"%6i\n",modus[i].com);
         }
        if (modus[i].enable_set != 0)
         {
          fprintf(outfile,"%12i  enable line # and values\n",modus[i].enable_set);
          k = 0;
          for (j=0; j < sizeof(modus[i].enable)/sizeof(int); j++)
           {
             fprintf(outfile,"%6i",modus[i].enable[j]);
             k++;
             if ((k % 8) == 0) fprintf(outfile,"\n");
           }
         }
        if (modus[i].gen_set != 0)
         {
          fprintf(outfile,"%12i  gen line # and value\n",modus[i].gen_set);
          fprintf(outfile,"%6i\n",modus[i].gen_type);
         }
        if (modus[i].delay_set != 0)
         {
          fprintf(outfile,"%12i  delay line # and values\n",modus[i].delay_set);
          k = 0;
          for (j=0; j < sizeof(modus[i].delay)/sizeof(int); j++)
           {
             fprintf(outfile,"%6i",modus[i].delay[j]);
             k++;
             if ((k % 8) == 0) fprintf(outfile,"\n");
           }
         }
        if (modus[i].mode_set != 0)
         {
          fprintf(outfile,"%12i  mode line # and value\n",modus[i].mode_set);
          fprintf(outfile,"%6i\n",modus[i].mode);
         }
        if (modus[i].ovfl_set != 0)
         {
          fprintf(outfile,"%12i  overflow line # and value\n",modus[i].ovfl_set);
          fprintf(outfile,"%6i\n",modus[i].ovfl);
         }
       }
}
/******************************************************************************
******************************************************************************/
void inhibit(char *func)
{

static int  i,j,stat,mode,count;
static int  crates[8];
static int  inhib[8],cdat;

      for (i=0; i <= 7; i++) crates[i] = -1;
      for (i=0; i <= m_index; i++)if (modus[i].c != -1) crates[modus[i].c] = 1;
      cn[0] = 30;
      ca[0] = 0;
      cf[0] = 1;
      mode = 1;
      count = 1;
      if (!strcmp(func,"save"))
       {
        for (i=0; i <= 7; i++)
         {
          inhib[i] = -1;
          if (crates[i] >= 0)
           {
            cc[0] = i;
            camacio_(&mode,cc,cn,ca,cf,&inhib[i],&count,&stat);
            inhib[i] = (inhib[i] & 4);
            if (stat != 0) exit(99);
           }
         }
        cf[0] = 17;
        for (i=0; i <= 7; i++)
         {
          cdat = 4;
          cc[0] = i;
          if (inhib[i] == 0) camacio_(&mode,cc,cn,ca,cf,&cdat,&count,&stat);
         }
       }
      else
       {
        cf[0] = 17;
        for (i=0; i <= 7; i++)
         {
          cdat = 0;
          cc[0] = i;
          if (inhib[i] == 0) camacio_(&mode,cc,cn,ca,cf,&cdat,&count,&stat);
         }
       }
}
/******************************************************************************
******************************************************************************/
void format_silena(void)
{

static int i,j,k;
static float  val;


      for (j=0; j <= m_index; j++)
       {
        if (modus[j].mt_type == SILENA)
         {
          if (modus[j].low_set != 0)
           {
            for (k=0; k < 8; k++);
             {
              val = modus[j].low[k];
              val = val * 0.255 +0.5;
              modus[j].low[k] = val;
              if (modus[j].low[k] < 0) modus[j].low[k] = 0;
              if (modus[j].low[k] > 255) modus[j].low[k] = 255;
             }
           }
          else
           {
            for (k=0; k < 8; k++) modus[j].low[k] = 0;
            modus[j].low_set = modus[j].mt_line;
           }
          if (modus[j].up_set != 0)
           {
            for (k=0; k < 8; k++)
             {
              val = modus[j].up[k];
              val = val - 8500;
              val = val * 0.17 + 0.5;
              modus[j].up[k] = val;
              if (modus[j].up[k] < 0) modus[j].up[k] = 0;
              if (modus[j].up[k] > 255) modus[j].up[k] = 255;
             }
           }
          else
           {
            for (k=0; k < 8; k++) modus[j].up[k] = 255;
            modus[j].up_set = modus[j].mt_line;
           }
          if (modus[j].off_set != 0)
           {
            for (k=0; k < 8; k++)
             {
              val = modus[j].off[k];
              val = val + 122.88;
              val = (val * 255.0)/245.76 + 0.5;
              modus[j].off[k] = val;
              if (modus[j].off[k] < 0) modus[j].off[k] = 0;
              if (modus[j].off[k] > 255) modus[j].off[k] = 255;
             }
           }
          else
           {
            for (k=0; k < 8; k++) modus[j].off[k] = 128;
            modus[j].off_set = modus[j].mt_line;
           }
          if (modus[j].com_set != 0)
           {
            val = modus[j].com;
            val = val * 0.2125 + 0.5;
            modus[j].com = val;
            if (modus[j].com < 0) modus[j].com = 0;
            if (modus[j].com > 255) modus[j].com = 255;
           }
          else
           {
            modus[j].com = 21;
            modus[j].com_set = modus[j].mt_line;
           }
         }
       }   
      return;

}
/******************************************************************************
******************************************************************************/
void clear_silena(int *err)
{

static int i,j,k;
static int crates[8],slot[8];


      for (i=0; i < 8; i++) crates[i] = -1;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type == SILENA)
         {
          crates[modus[i].c] = 1;
          slot[modus[i].c] = modus[i].n;
         }
       }
      for (i=0; i < 8; i++)
       {
        if (crates[i] > 0)
         {
          for (j=0; j < 12; j++)
           {
            cc[j] = i;
            cn[j] = slot[i];
            ca[j] = 0;
            cf[j] = 0;
            wr[j] = 0;
           }
          cn[0] = 30;
          cn[11] = 30;
          cf[0] = 17;
          cf[11] = 17;
          wr[11] = 4;
          cam_mode = 0;
          cam_count = 12;
          camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
         }
       }
}
/******************************************************************************
******************************************************************************/
void load_phil(int *err)
{

static int    err_count;
static int    i,j,k;
static unsigned short  mode_wd;

      if (m_index < 0) return;
      for (i-0; i <= m_index; i++)
       {
        if (modus[i].mt_type != PHIL) goto L100;
        printf("$$ Setup Phillips Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        j = 0;
        mode_wd = 0;
        for (k=0; k < 400; k++)
         {
          cc[k] = modus[i].c;
          cn[k] = modus[i].n;
          ca[k] = 0;
          cf[k] = 20;
         }
        cf[j] = 9;
        j = j + 1;
        if (modus[i].low_set != 0)
         {
          ca[j] = 1;
          cf[j] = 17;
          wr[j] = 0;
          j = j + 1;
          for (k=0; k < 16; k++)
           {
            ca[j] = k;
            wr[j] = modus[i].low[k];
            j = j + 1;
           }
          mode_wd = mode_wd | 2;
         }
        if (modus[i].up_set != 0)
         {
          ca[j] = 2;
          cf[j] = 17;
          wr[j] = 0;
          j = j + 1;
          for (k=0; k < 16; k++)
           {
            ca[j] = k;
            wr[j] = modus[i].up[k];
            j = j + 1;
           }
          mode_wd = mode_wd | 4;
         }
        if (modus[i].ped_set != 0)
         {
          ca[j] = 0;
          cf[j] = 17;
          wr[j] = 0;
          j = j + 1;
          for (k=0; k < 16; k++)
           {
            ca[j] = k;
            wr[j] = modus[i].ped[k] & 0x1FFF;
            j = j + 1;
           }
          mode_wd = mode_wd | 1;
         }
        cam_mode = 0;
        cam_count = j;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        err_count = 5;
        for (k=0; k < j; k++)
         {
          if (st[k] != 0)
           {
            *err = 1;
            cam_error(cc[k],cn[k],ca[k],cf[k],st[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
        for (k=0; k < j; k++)
         {
          if (cf[k] == 20) cf[k] = 1;
         }
        err_count = 5;
        cam_mode = 0;
        cam_count = j;
        camlist_(cc,cn,ca,cf,&cam_mode,rd,&cam_count,st);
        for (k=0; k < j; k++)
         {
          if (st[k] != 0)
           {
            *err = 1;
            cam_error(cc[k],cn[k],ca[k],cf[k],st[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
        err_count = 5;
        for (k=0; k < j; k++)
         {
          if (cf[k] == 1)
           {
            if (rd[k] != wr[k])
             {
              *err = 1;
              verify_error(cc[k],cn[k],ca[k],cf[k],wr[k],rd[k]);
              err_count = err_count - 1;
              if (err_count <= 0) goto L100;
             }
           }
         }
        cf[0] = 19;
        ca[0] = 0;
        wr[0] = mode_wd;
        cf[1] = 6;
        ca[1] = 0;
        cam_mode = 0;
        cam_count = 2;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        if (st[0] != 0)
         {
          cam_error(cc[0],cn[0],ca[0],cf[0],st[0]);
         }
        if (st[1] != 0)
         {
          cam_error(cc[1],cn[1],ca[1],cf[1],st[1]);
         }
        if (mode_wd != (wr[1] & 15))
         {
          *err = 1;
          verify_error(cc[1],cn[1],ca[1],cf[1],wr[0],wr[1]);
         }
L100:
        continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_silena(int *err)
{
static int   err_count;
static int   i,j,k;
static int   mode_wd;

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != SILENA) goto L100;
        printf("$$ Setup Silena Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        if (modus[i].mode_set != 0)
         {
          mode_wd = (modus[i].mode & 0x7EFF);
         }
        else
         {
          mode_wd = 0xA00;
         }
        if (modus[i].ovfl_set != 0)
         {
          if (modus[i].ovfl != 0)
           {
            mode_wd = (mode_wd & 0x76ff);;
           }
          else
           {
            mode_wd = (mode_wd | 0x800);
           }
         }
        j = 0;
        for (k=0; k < 400; k++)
         {
          cc[k] = modus[i].c;
          cn[k] = modus[i].n;
          ca[k] = 0;
         }
        cf[j] = 9;
        j = j + 1;
        if (modus[i].low_set != 0)
         {
          for (k=0; k < 8; k++)
           {
            ca[j] = k + 8;
            cf[j] = 17;
            wr[j] = modus[i].low[k];
            j = j + 1;
           }
         }
        if (modus[i].up_set != 0)
         {
          for (k=0; k < 8; k++)
           {
            ca[j] = k;
            cf[j] = 17;
            wr[j] = modus[i].up[k];
            j = j + 1;
           }
         }
        if (modus[i].off_set != 0)
         {
          for (k=0; k < 8; k++)
           {
            ca[j] = k;
            cf[j] = 20;
            wr[j] = modus[i].off[k];
            j = j + 1;
           }
         }
        if (modus[i].com_set != 0)
         {
          ca[j] = 9;
          cf[j] = 20;
          wr[j] = modus[i].com;
          j = j + 1;
         }
        cam_mode = 0;
        cam_count = j;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        err_count = 5;
        for (k=0; k < j; k++)
         {
          if (st[k] != 0)
           {
            *err = 1;
            cam_error(cc[k],cn[k],ca[k],cf[k],st[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
        for (k=0; k < j; k++)
         {
          if (cf[k] == 20) cf[k] = 4;
          if (cf[k] == 17) cf[k] = 1;
         }
        err_count = 5;
        cam_mode = 0;
        cam_count = j;
        camlist_(cc,cn,ca,cf,&cam_mode,rd,&cam_count,st);
        for (k=0; k < j; k++)
         {
          if (st[k] != 0)
           {
            *err = 1;
            cam_error(cc[k],cn[k],ca[k],cf[k],st[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
        err_count = 500;
        for (k=0; k < j; k++)
         {
          if (cf[k] == 1 || cf[k] == 4)
           {
            if (rd[k] != wr[k])
             {
              *err = 1;
              verify_error(cc[k],cn[k],ca[k],cf[k],wr[k],rd[k]);
              err_count = err_count - 1;
              if (err_count <= 0) goto L100;
             }
           }
         }
        cf[0] = 20;
        ca[0] = 14;
        wr[0] = mode_wd;
        cf[1] = 4;
        ca[1] = 14;
        cam_mode = 0;
        cam_count = 2;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        if (st[0] != 0)
         {
          cam_error(cc[0],cn[0],ca[0],cf[0],st[0]);
         }
        if (st[1] != 0)
         {
          cam_error(cc[1],cn[1],ca[1],cf[1],st[1]);
         }
        if (mode_wd != (wr[1] & 0xfeff))
         {
          *err = 1;
          verify_error(cc[1],cn[1],ca[1],cf[1],wr[0],wr[1]);
         }
L100:    continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_hhirf_adc(int *err)
{

static int     err_count;
static int     i,j,k;
static unsigned short   enables,mask;
static int     c,n,stat;

      if (m_index < 0) return;
      for (i=0; i < m_index; i++)
       {
        if (modus[i].mt_type != HHIRF_ADC) goto L100;
        printf("$$ Setup HHIRF_ADC Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        c = modus[i].c;
        n = modus[i].n;
        cam_mode = 0;
        cama = 0;
        camf = 9;
        cam_count = 1;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&cam_mode,&cam_count,&stat);
        if (stat != 0) *err = 1;
        enables = 0;
        mask = 1;
        if (modus[i].enable_set != 0)
         {
          for (k=0; k < 16; k++)
           {
            if (modus[i].enable[k] != 0) enables = (enables | mask);
            mask = mask * 2;
           }
         }
        camf = 17;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&enables,&cam_count,&stat);
        if (stat != 0) *err = 1;
        camf = 9;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&cam_mode,&cam_count,&stat);
        if (stat != 0) *err = 1;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&cam_mode,&cam_count,&stat);
        if (stat != 0) *err = 1;
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_ue_clock(int *err)
{

static int     err_count;
static int     i,j,k;
static unsigned short   mode_wd,temp;
static int     c,n,stat;


      if (m_index < 0) return;
      for (i=0; i < m_index; i++)
       {
        if (modus[i].mt_type != UE_CLOCK) goto L100;
        printf("$$ Setup UE_clock Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        c = modus[i].c;
        n = modus[i].n;
/*
*   Clear data register and R2 of the control register
*/
        
        cam_mode = 0;
        cama = 0;
        camf = 9;
        cam_count = 1;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&cam_mode,&cam_count,&stat);
        if (stat != 0) *err = 1;
/*
*   If mode was not specified, the default is Mode 1
*/
        if (modus[i].mode_set != 0)
         {
          mode_wd = modus[i].mode;
         }
        else
         {
          mode_wd = 1;
         }
        camf = 17;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&mode_wd,&cam_count,&stat);
        if (stat != 0) *err = 1;
        camf = 1;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&temp,&cam_count,&stat);
        if (stat != 0) *err = 1;
        temp = (temp & 1);
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_lrs4300(int *err)
{
static int  err_count;
static int  i,j,k;
static int  mode_wd;


      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != LRS_4300) goto L100;
        printf("$$ Setup LRS_4300 Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        if (modus[i].mode_set != 0) mode_wd = modus[i].mode;
        else mode_wd = 0;
        j = 0;
        for (k=0; k < 400; k++)
         {
          cc[k] = modus[i].c;
          cn[k] = modus[i].n;
          ca[k] = 0;
         }
        cf[j] = 9;
        j = j + 1;
        if (modus[i].ped_set != 0)
         {
          for (k=0; k < 16; k++)
           {
            ca[j] = k;
            cf[j] = 17;
            wr[j] = modus[i].ped[k];
            j = j + 1;
           }
          mode_wd = (mode_wd | 0x900);   /* turn on pedestal subtraction */
         }
        else
         {
          mode_wd = (mode_wd & 0xf6ff); /* turn off pedestal subtraction  */
         }
        if (modus[i].ovfl_set != 0)
         {
          mode_wd = (mode_wd & 0x7fff);
          if (modus[i].ovfl != 0) mode_wd = (mode_wd | 0x8000);
         }
        cam_mode = 0;
        cam_count = j;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        err_count = 5;
        for (k=0; k < j; k++)
         {
          if (st[k] != 0)
           {
            *err = 1;
            cam_error(cc[k],cn[k],ca[k],cf[k],st[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
        for (k=0; k < j; k++) if (cf[k] == 17) cf[k] = 1;
        err_count = 5;
        cam_mode = 0;
        cam_count = j;
        camlist_(cc,cn,ca,cf,&cam_mode,rd,&cam_count,st);
        for (k=0; k < j; k++)
         {
          if (st[k] != 0)
           {
            *err = 1;
            cam_error(cc[k],cn[k],ca[k],cf[k],st[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
        err_count = 5;
        for (k=0; k < j; k++)
         {
          if (cf[k] == 1)
           {
            if (rd[k] != wr[k])
             {
              *err = 1;
              verify_error(cc[k],cn[k],ca[k],cf[k],wr[k],rd[k]);
              err_count = err_count - 1;
              if (err_count <= 0) goto L100;
             }
           }
         }
        cf[0] = 16;
        ca[0] = 0;
        wr[0] = mode_wd;
        cf[1] = 0;
        ca[1] = 0;
        cam_mode = 0;
        cam_count = 2;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        if (wr[0] != wr[1])
         {
          *err = 1;
          verify_error(cc[1],cn[1],ca[1],cf[1],wr[0],wr[1]);
         }
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_lrs4418(int *err)
{
static int     i,j,k;

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != LRS_4418) goto L100;
        printf("$$ Setup LRS_4418 Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        j = 0;
        for (k=0; k < 400; k++)
         {
          cc[k] = modus[i].c;
          cn[k] = modus[i].n;
          ca[k] = 0;
         }
        if (modus[i].delay_set != 0)
         {
          for (k=0; k < 16; k++)
           {
            ca[j] = k;
            cf[j] = 16;
            wr[j] = modus[i].delay[k]/8;
            if (wr[j] > 15) wr[j] = 15;
            j = j + 1;
           }
         }
        else
         {
          for (k=0; k < 16; k++)
           {
            ca[j] = k;
            cf[j] = 16;
            wr[j] = 0;
            j = j + 1;
           }
         }
        cam_mode = 0;
        cam_count = j;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        err_count = 5;
        for (k=0; k < j; k++)
         {
          if (st[k] != 0)
           {
            *err = 1;
            cam_error(cc[k],cn[k],ca[k],cf[k],st[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_phil7106(int *err)
{
static int     i,j,k;
static int     fmode,mask,ena_wd;

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != PHIL_7106) goto L100;
        printf("$$ Setup phil_7106 Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        j = 0;
        for (k=0; k < 32; k++)
         {
          cc[k] = modus[i].c;
          cn[k] = modus[i].n;
         }
        if (modus[i].com_set != 0)
         {
          ca[j] = 0;
          cf[j] = 17;
          wr[j] = modus[i].com;             /* set threshold DAC  */
          j = j + 1;
          ca[j] = 0;
          cf[j] = 26;                        /* set mode to Remote   */
          j = j + 1;
          fmode = 0;
         }
        else
         {
          ca[j] = 0;
          cf[j] = 24;                        /* set mode to Local    */
          j = j + 1;
          fmode = 0x8000;
         }
        if (modus[i].enable_set != 0)
         {
          mask = 1;
          ena_wd = 0;
          for (k=0; k < 16; k++)
           {
            if (modus[i].enable[k] != 0) ena_wd = (ena_wd | mask);
            mask = mask * 2;
           }
         }
        else
         {
          ena_wd = 0xFFFF;
         }
        ca[j] = 0;
        cf[j] = 16;
        wr[j] = ena_wd;                      /* set Mask register        */
        j = j + 1;
        cam_mode = 0;
        cam_count = j;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        err_count = 5;
        for (k=0; k < j; k++)
         {
          if (st[k] != 0)
           {
            *err = 1;
            cam_error(cc[k],cn[k],ca[k],cf[k],st[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
        ca[0] = 1;
        cf[0] = 17;                          /* start threshold ADC conv  */
        wr[0] = 0;
        cam_mode = 0;
        cam_count = 1;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        if (st[0] != 0)
         {
          cam_error(cc[0],cn[0],ca[0],cf[0],st[0]);
          err_count = err_count - 1;
         }
        ca[0] = 1;
        cf[0] = 1;                           /* read threshold and mode  */
        cam_mode = 0;
        cam_count = 1;
        camlist_(cc,cn,ca,cf,&cam_mode,rd,&cam_count,st);
        if (st[0] != 0)
         {
          cam_error(cc[0],cn[0],ca[0],cf[0],st[0]);
          err_count = err_count - 1;
         }
        mask = rd[0];
        mask = (mask & 0x8000);
        if (mask != fmode)
         {
          if (mask == 0)
           {
            printf(" MODE ERROR: Should be Local but is Remote\n");
           }
          else
           {
            printf(" MODE ERROR: Should be Remote but is Local\n");
           }
         }
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_lrs4415a(int *err)
{
static int     i,j,k;
static unsigned short   enables,mask,imode;
static int     c,n,stat;

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != LRS_4415A) goto L100;
        printf("$$ Setup lrs_4415a Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        c = modus[i].c;
        n = modus[i].n;
        enables = 0;
        mask = 1;
        if (modus[i].enable_set != 0)
         {
          for (k=0; k < 16; k++)
           {
            if (modus[i].enable[k] == 0) enables = (enables | mask);
            mask = mask * 2;
           }
         }
        imode = 0;
        if (modus[i].mode_set != 0)
         {
          if (modus[i].mode != 0) imode = 1;
         }
        cam_mode = 0;
        cama = 0;
        camf = 17;
        cam_count = 1;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&imode,&cam_count,&stat);
                                                              /* set mode   */
        if (stat != 0) *err = 1;
        camf = 16;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&enables,&cam_count,&stat);
                                                    /* write mask register  */
        if (stat != 0) *err = 1;
        camf = 27;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&enables,&cam_count,&stat);
                                                       /* test mode switch  */
        if (stat == NOQ)
         {
          printf("** WARNING ** lrs_4415a Module: C = %1i N = %2i is in LOCAL mode\n",modus[i].c,modus[i].n);
         }
        else if (stat != 0)
         {
          *err = 1;
         }
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_lrs3511(int *err)
{
static int     i,j,k;
static unsigned short   imode,temp;
static int     c,n,stat;
static int     gain[6] = {8192,4096,2048,1024,512,256};

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != LRS_3511) goto L100;
        printf("$$ Setup lrs_3511 Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        c = modus[i].c;
        n = modus[i].n;
        imode = 0;
        if (modus[i].mode_set != 0)
         {
          imode = (imode | modus[i].mode);
         }
        else
         {
          imode = (imode | 0x100);
         }
        if (modus[i].off_set != 0)
         {
          temp = gain[(imode/256)-1];
          temp = (modus[i].off[0]/temp)*temp;
          imode = (imode | (temp/256));
         }
        cam_mode = 0;
        cama = 0;
        camf = 16;
        cam_count = 1;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&imode,&cam_count,&stat);
                                                                 /* set mode  */
        if (stat != 0) *err = 1;
        camf = 26;
        cam_count = 0;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&cam_count,&cam_count,&stat);
                                              /* Some 3511s need to be enabled*/
        if (stat != 0) *err = 1;
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_lrs4413(int *err)
{
static int     i,j,k,local;
static int     mask,ena_wd;

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != LRS_4413) goto L100;
        printf("$$ Setup lrs_4413 Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        local = 1;
        j = 0;
        for (k=0; k < 32; k++)
         {
          cc[k] = modus[i].c;
          cn[k] = modus[i].n;
         }
        ca[j] = 0;
        cf[j] = 26;            /* set remote mode */
        j = j + 1;
        if (modus[i].com_set != 0)
         {
          local = 0;
          if (modus[i].com == 0)
           {
            wr[j] = 1024;
           }
          else
           {
            wr[j] = modus[i].com;
           }
         }
        else
         {
          wr[j] = 1024;
         }
        ca[j] = 0;
        cf[j] = 17;
        j = j + 1;
        if (modus[i].enable_set != 0)
         {
          local = 0;
          mask = 1;
          ena_wd = 0;
          for (k=0; k < 16; k++)
           {
            if (modus[i].enable[k] == 0) ena_wd = (ena_wd | mask);
            mask = mask * 2;
           }
         }
        else
         {
          ena_wd = 0;
         }
        ca[j] = 0;
        cf[j] = 16;
        wr[j] = ena_wd;                      /* set Mask register  */
        j = j + 1;
        cam_count = j;
        cam_mode = 0;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        err_count = 5;
        for (k=0; k < j; k++)
         {
          if (st[k] != 0)
           {
            *err = 1;
            cam_error(cc[k],cn[k],ca[k],cf[k],st[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
        j = 0;
        ca[j] = 0;
        cf[j] = 1;                           /* read threshold  */
        j = j + 1;
        ca[j] = 0;
        cf[j] = 0;                           /* read mask  */
        j = j + 1;
        if (local == 1)
         {
          ca[j] = 0;
          cf[j] = 24;
          j = j + 1;
         }
        err_count = 5;
        cam_count = j;
        cam_mode = 0;
        camlist_(cc,cn,ca,cf,&cam_mode,rd,&cam_count,st);
        if (st[0] != 0);
         {
          cam_error(cc[0],cn[0],ca[0],cf[0],st[0]);
          err_count = err_count - 1;
         }
        for (k=0; k < 2; k++)
         {
          if (rd[k] != wr[k])
           {
            *err = 1;
            verify_error(cc[k],cn[k],ca[k],cf[k],wr[k],rd[k]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_lrs4416b(int *err)
{
static int     i,j,k;
static unsigned short   enables,mask,temp,thresh;
static int     c,n,stat;

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != LRS_4416B) goto L100;
        printf("$$ Setup lrs_4416b Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        c = modus[i].c;
        n = modus[i].n;
        enables = 0;
        mask = 1;
        if (modus[i].enable_set != 0)
         {
          for (k=0; k < 16; k++)
           {
            if (modus[i].enable[k] == 0) enables = (enables | mask);
            mask = mask * 2;
           }
         }
        cam_mode = 0;
        cama = 0;
        camf = 16;
        cam_count = 1;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&enables,&cam_count,&stat);
                                                      /* write mask register */
        if (stat != 0) *err = 1;
        camf = 1;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&temp,&cam_count,&stat);
                                                      /* read mask register  */
        if (stat != 0) *err = 1;
        if (enables != temp)
         {
          verify_error(c,n,0,1,enables,temp);
          *err = 1;
         }
        if (modus[i].com_set != 0)
         {
          if (modus[i].com == 0)
           {
            thresh = 1024;
           }
          else
           {
            thresh = modus[i].com;
           }
         }
        else
         {
          thresh = 1024;
         }
        camf = 17;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&thresh,&cam_count,&stat);
                                                          /* write threshold */
        if (stat != 0) *err = 1;
        camf = 27;
        camacio_(&cam_mode,&c,&n,&cama,&camf,&enables,&cam_count,&stat);
                                                         /* test mode switch */
        if (stat == NOQ)
         {
          printf("** WARNING ** lrs_4416b Module: C = %1i, N = %2i is in LOCAL mode\n",modus[i].c,modus[i].n);
         }
        else if (stat != 0)
         {
          *err = 1;
         }
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_lrs3377(int *err)
{
static int     i,j,k;

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != LRS_3377) goto L100;
        printf("$$ Setup LeCroy 3377 Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        j = 0;
        for (k=0; k < 400; k++)
         {
          cc[k] = modus[i].c;
          cn[k] = modus[i].n;
          ca[k] = 0;
          cf[k] = 0;
         }
        cf[j] = 9;
        j = j + 1;
        cf[j] = 30;
        j = j + 1;
        if (modus[i].com_set == 0) modus[i].com = 0;
        if (modus[i].com == 0)
         {
          cf[j] = 23;          /* Mode 3, common start, double word mode */
         }
        else
         {
          cf[j] = 22;          /* Mode 2, common stop, double word mode */
         }
        j = j + 1;
        cf[j] = 25;
        j = j + 1;
        cam_count = j;
        cam_mode = 0;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        sleep(2);
        j = 0;
        cf[j] = 9;
        j = j + 1;
        cf[j] = 24;
        ca[j] = 1;
        j = j + 1;
        cam_count = j;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        j = 0;
        if (modus[i].up_set == 0) modus[i].up[0] = 32767;
        if (modus[i].com == 0)
         {
           cf[j] = 17;          /* Common start mode */
           ca[j] = 0;
           wr[j] = 0xe000;  /* rising edge only, single buffer, skip header */
           j = j + 1;
           cf[j] = 17;
           ca[j] = 1;
           wr[j] = 0;           /* no MPI */
           j = j + 1;
           cf[j] = 17;
           ca[j] = 2;
           wr[j] = 0;           /* Max 16 hits per channel */
           j = j + 1;
           cf[j] = 17;
           ca[j] = 3;
           if (modus[i].up[0] >= 10000)  /* compute request dlytime */
            {
             wr[j] = 0;
            }
           else
            {
             wr[j] = 10 - modus[i].up[0]/1000;
             wr[j] = wr[j]/2 + 1;
            }
           j = j + 1;
           cf[j] = 17;
           ca[j] = 4;
           wr[j] = modus[i].up[0]/50;
           j = j + 1;
           cf[j] = 17;
           ca[j] = 5;
           wr[j] = 0;           /* test mode disabled */
           j = j + 1;
         }
        else
         {
           cf[j] = 17;          /* Common stop mode */
           ca[j] = 0;
           wr[j] = 0xa000;   /* rising edge only, single buffer, skip header */
           j = j + 1;
           cf[j] = 17;
           ca[j] = 1;
           wr[j] = 2;           /* trigger outputs width 50 nanoseconds */
           j = j + 1;
           cf[j] = 17;
           ca[j] = 2;
           wr[j] = (modus[i].up[0]/8)*16;
           j = j + 1;
           cf[j] = 17;
           ca[j] = 3;
           if (modus[i].up[0] >= 10000)  /* compute request dlytime */
            {
             wr[j] = 0;
            }
           else
            {
             wr[j] = 10 - modus[i].up[0]/1000;
             wr[j] = wr[j]/2 + 1;
            }
           j = j + 1;
         }
        cf[j] = 26;
        ca[j] = 1;
        j = j + 1;
        cam_count = j;
        cam_mode = 0;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        for (k=0; k < j-1; k++)
         {
          cf[k] = 1;
         }
        cam_count = j-1;
        cam_mode = 0;
        camlist_(cc,cn,ca,cf,&cam_mode,rd,&cam_count,st);
        rd[2] = (rd[2] & 0x1fff);
        if (modus[i].com == 0) rd[3] = (rd[3] & 0x000f);
        for (k=0; k < j-1; k++)
         {
          if (rd[k] != wr[k])
           {
            *err = 1;
            verify_error(cc[k],cn[k],ca[k],cf[k],wr[k],rd[k]);
           }
         }
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_shaper(int *err)
{
static int     i,j,k,l;
static int     value,dat;

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != SHAPER) goto L100;
        printf("$$ Setup Shaper Module: C = %1i, N = %2i\n",modus[i].c,modus[i].n);
        if (modus[i].ped_set == 0)
         {
          for (k=0; k < 16; k++)
           {
            modus[i].ped[k] = 255;
           }
         }
        for (k=0; k < 400; k++)
         {
          cc[k] = modus[i].c;
          cn[k] = modus[i].n;
          ca[k] = 1;
          cf[k] = 16;
         }
        k = 0;
        for (l=15; l > 0; l -= 2)
         {
          wr[k] = 4;
          k = k + 1;
          wr[k] = 6;
          k = k + 1;
          wr[k] = 4;
          k = k + 1;
          value = modus[i].ped[l];
          for (j=0; j < 8; j++)
           {
            if ((value & 0x80) == 0)
             {
              dat = 4;
             }
            else
             {
              dat = 5;
             }
            wr[k] = dat;
            k = k + 1;
            wr[k] = dat + 2;
            k = k + 1;
            value = value * 2;
           }
          value = modus[i].ped[l-1];
          for (j=0; j < 8; j++)
           {
            if ((value & 0x80) == 0)
             {
              dat = 4;
             }
            else
             {
              dat = 5;
             }
            wr[k] = dat;
            k = k + 1;
            wr[k] = dat + 2;
            k = k + 1;
            value = value * 2;
           }
         }
        wr[k] = 0;
        cam_mode = 0;
        cam_count = k;
        camlist_(cc,cn,ca,cf,&cam_mode,wr,&cam_count,st);
        err_count = 5;
        for (j=0; j < k; j++)
         {
          if (st[j] != 0)
           {
            *err = 1;
            cam_error(cc[j],cn[j],ca[j],cf[j],st[j]);
            err_count = err_count - 1;
            if (err_count <= 0) goto L100;
           }
         }
L100:   continue;
       }
}

/******************************************************************************
******************************************************************************/
void load_caen775(int *err)
{
static int     err_count;
static int     i,j;
static int     temp[32],range,wmode,xrange,xmode;
static float   a,b,ftmp;
static char    errmsg[80];

      if (m_index < 0) return;
      a = 35660.3774;
      b = 0.2830;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != CAEN_775) goto L100;
        printf("$$ Setup CAEN 775 VME TDC Module: Num = %2i\n",modus[i].n);
        if (modus[i].com_set == 0) modus[i].com = 0;
        if (modus[i].low_set == 0)
         {
          for (j=0; j < 32; j++)
           {
            modus[i].low[j] = 0;
           }
         }
        wmode = modus[i].com;
        if (modus[i].up_set == 0) modus[i].up[0] = 1200;
        ftmp = modus[i].up[0];
        ftmp = a/ftmp + b;
        range = ftmp + 0.5;
        for (j=0; j < 32; j++)
         {
          ftmp = modus[i].low[j];
          ftmp = ftmp/16.0;
          modus[i].low[j] = ftmp + 0.5;
         }
        caen_tdc_write_(&modus[i].n,&modus[i].low,&range,&wmode,err);
        if (*err != 0)
         {
          caen_error(err,errmsg,sizeof(errmsg));
          printf("%s\n",errmsg);
         }
        caen_tdc_read_(&modus[i].n,&temp,&xrange,&xmode,err);
        if (*err != 0)
         {
          caen_error(err,errmsg,sizeof(errmsg));
          printf("%s\n",errmsg);
         }
        if (xrange != range)
         {
        printf("CAEN TDC Num = %2i range: wrote =%3i read = %3i\n",modus[i].n,range,xrange);
         }
        if (xmode != wmode)
         {
          *err = 1;
          if (wmode != 0)
           {
            printf("CAEN TDC Num = %2i wrote common stop --  read common start\n",modus[i].n);
           }
          else
           {
            printf("CAEN TDC Num = %2i wrote common start --  read common stop\n",modus[i].n);

           }
         }
        err_count = 5;
        for (j=0; j < 32; j++)
         {
          if (modus[i].low[j] != temp[j])
           {
            *err = 1;
            printf("CAEN TDC Num = %2i Channel = $4i  wrote = %4i read = %4i\n",modus[i].n,j,modus[i].low[j],temp[j]);
            err_count = err_count -1;
            if (err_count <= 0) goto L100;
           }
         }
L100:   continue;
       }
}

/******************************************************************************
******************************************************************************/
void load_caen785(int *err)
{
static int     err_count;
static int     i,j;
static int     temp[32];
static float   ftmp;
static char    errmsg[80];

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != CAEN_785) goto L100;
        printf("$$ Setup CAEN 785 VME ADC Module: Num = %2i\n",modus[i].n);
        if (modus[i].low_set == 0)
         {
          for (j=0; j < 32; j++)
           {
            modus[i].low[j] = 0;
           }
         }
        for (j=0; j < 32; j++)
         {
          ftmp = modus[i].low[j];
          ftmp = ftmp/16.0;
          modus[i].low[j] = ftmp + 0.5;
         }
        caen_adc_write_(&modus[i].n,&modus[i].low,err);
        if (err != 0)
         {
          caen_error(err,errmsg,sizeof(errmsg));
          printf("%s\n",errmsg);
         }
        caen_adc_read_(&modus[i].n,&temp,err);
        if (*err != 0)
         {
          caen_error(err,errmsg,sizeof(errmsg));
          printf("%s\n",errmsg);
         }
        err_count = 5;
        for (j=0; j < 32; j++)
         {
          if (modus[i].low[j] != temp[j])
           {
            *err = 1;
            printf("CAEN ADC Num = %2i Channel = %4i wrote = %4i read = %4i\n",modus[i].n,j,modus[i].low[j],temp[j]);
            err_count = err_count -1;
            if (err_count <= 0) goto L100;
           }
         }
L100:   continue;
       }
}
/******************************************************************************
******************************************************************************/
void load_caen792(int *err)
{
static int     err_count;
static int     i,j;
static int     temp[32],iped,xped;
static float   ftmp;
static char    errmsg[80];

 if (m_index < 0) return;
 for (i=0; i <= m_index; i++)
   {
     if (modus[i].mt_type != CAEN_792) goto L100;
     printf("$$ Setup CAEN 792 VME QDC Module: Num = %2i\n",modus[i].n);
     if (modus[i].low_set == 0)
       {
	 for (j=0; j < 32; j++)
           {
	     modus[i].low[j] = 0;
           }
       }
     if (modus[i].ped_set == 0) modus[i].ped[0] = 45;
     iped = modus[i].ped[0];
     for (j=0; j < 32; j++)
       {
	 ftmp = modus[i].low[j];
	 ftmp = ftmp/16.0;
	 modus[i].low[j] = ftmp + 0.5;
       }
     caen_qdc_write_(&modus[i].n,&modus[i].low,&iped,err);
     if (*err != 0)
       {
	 caen_error(err,errmsg,sizeof(errmsg));
	 printf("QDC Write: %s\n",errmsg);
       }
     caen_qdc_read_(&modus[i].n,&temp,&xped,err);
     if (*err != 0)
       {
	 caen_error(err,errmsg,sizeof(errmsg));
	 printf("QDC Read: %s\n",errmsg);
       }
     if (xped != iped)
       {
	 printf("CAEN QDC Num = %2i iped: wrote =%3i read = %3i\n",modus[i].n,iped,xped);
       }
     err_count = 5;
     for (j=0; j < 32; j++)
       {
	 if (modus[i].low[j] != temp[j])
           {
	     *err = 1;
	     printf("CAEN QDC Num = %2i Channel = $4i  wrote = %4i read = %4i\n",
		    modus[i].n, j, modus[i].low[j], temp[j]);
	     err_count = err_count -1;
	     if (err_count <= 0) goto L100;
           }
       }
   L100:   continue;
   }
}
/******************************************************************************
******************************************************************************/


void camerr_(char *ipb,int *stat)
{

#include "cam_fast.h"

static int c,n,a,f;
static int i;
static char *msg[10] = {
       "GOOD X   BAD Q","GOOD Q   BAD X","BAD  Q   - BAD X",
       "Timeout",
       "Illegal Crate","Illegal Module","Illegal Address",
       "Illegal Function","Crate Off_Line","Unknown error code"};


      if(&*stat == 0) return;
/*
*  X = 1 and Q = 0 is ignored in this error handler
*/
      if(*stat == NOQ) return;   /* return if bad Q only */

      c = ipb[0];
      n = ipb[1];
      a = ipb[2];
      f = ipb[3];

      if (*stat == NOQ) i = 0;
      else if (*stat == NOX) i = 1;
      else if (*stat == NOX_NOQ) i = 2;
      else if (*stat == TIMEOUT) i = 3;
      else if (*stat == ILL_CRATE) i = 4;
      else if (*stat == ILL_MODULE) i = 5;
      else if (*stat == ILL_ADDRESS) i = 6;
      else if (*stat == ILL_FUNCTION) i = 7;
      else if (*stat == CRATE_OFF_LINE) i = 8;
      else i = 9;

      printf("CAM ERROR-C,N,A,F= %2i %2i %2i %2i  %s\n",c,n,a,f,msg[i]);

}
/******************************************************************************
******************************************************************************/
void verify_error(int c,int n,int a,int f,unsigned short wr,unsigned short rd)
{
      unsigned short diff;

      diff = wr ^ rd;
      printf("VERIFY ERROR-C,N,A,F= %2i %2i %2i %2i  WRITE,READ,DIFF= %5X %5X %5X\n",c,n,a,f,wr,rd,diff);
}
/******************************************************************************
******************************************************************************/
void cam_error(int c,int n,int a,int f,int stat)
{

#include "cam_fast.h"
   
      int i;
      char mess[64];
      char *msg[11] = {
       "dummy","GOOD X   BAD Q","GOOD Q   BAD X","BAD  Q   - BAD X",
       "Timeout",
       "Illegal Crate","Illegal Module","Illegal Address",
       "Illegal Function","Crate Off_Line","Unknown error code"};


      if(stat==0) return;    /*return if good     */

      if (stat == NOQ) i = 1;
      else if (stat == NOX) i = 2;
      else if (stat == NOX_NOQ) i = 3;
      else if (stat == TIMEOUT) i = 4;
      else if (stat == ILL_CRATE) i = 5;
      else if (stat == ILL_MODULE) i = 6;
      else if (stat == ILL_ADDRESS) i = 7;
      else if (stat == ILL_FUNCTION) i = 8;
      else if (stat == CRATE_OFF_LINE) i = 9;
      else i = 10;

      printf("CAM ERROR-C,N,A,F= %2i %2i %2i %2i  %s\n",c,n,a,f,msg[i]);
      return;
}
/******************************************************************************
******************************************************************************/
void load_lrs4508(int *err)
{
static int     i,j,k;
static int     c,n;
static char    cfunc[5];

      if (m_index < 0) return;
      for (i=0; i <= m_index; i++)
       {
        if (modus[i].mt_type != LRS_4508) continue;
        c = modus[i].c;
        n = modus[i].n;

        if (modus[i].gen_set != 0)
         {
          if (modus[i].gen_type == GE_ONLY) strcpy(cfunc,"GE");
          else if (modus[i].gen_type == BGO_ONLY) strcpy(cfunc,"BGO");
          else if (modus[i].gen_type == ANTI) strcpy(cfunc,"ANTI");
         }
        else strcpy(cfunc,"ANTI");
        m4508(&c,&n,cfunc);
       }
}
/******************************************************************************
******************************************************************************/
void m4508(int *c,int *n,char *cfunc)
{

static int addr,dat,func,erro,l,status;
static int array[256],iarray[256];
static char *function[3] = {"GE","BGO","ANTI"};

      printf("Loading LeCroy 4508 at C=%2i N=%2i Function= %s\n",*c,*n,cfunc);
      for (func=0; func < 3; func++)
       {
        if (!strcmp(cfunc,function[func])) goto L110;
       }
      printf("   4508 Function Specification Error - '%s'\n",cfunc);
      return;

L110: gen(array,&func,&erro);
      if (erro != 0) return;
      cam_mode = 0;
      cam_count = 0;
      cama = 2;
      camf = 9;
      camacio_(&cam_mode,c,n,&cama,&camf,&cam_count,&cam_count,&status);
                                                         /* CLEAR MEM ADDR */
      if ((status & 0xFFFF) != 0)
       {
         printf("   ABORTING ON CAMAC ERROR - F(9)A(2) - STATUS%9X\n",status);
         return;
       }
      cam_mode = 1;
      cam_count = 256;
      cama = 0;
      camf = 18;
      camacio_(&cam_mode,c,n,&cama,&camf,array,&cam_count,&status);
                                                             /* LOAD MEM  */
      if ((status & 0xFFFF) != 0)
       {
         printf("   ABORTING ON CAMAC ERROR - F(18)A(0) - STATUS%9X\n",status);
         return;
       }
      cam_mode = 0;
      cam_count = 0;
      cama = 2;
      camf = 9;
      camacio_(&cam_mode,c,n,&cama,&camf,&cam_count,&cam_count,&status);
                                                         /* CLEAR MEM ADDR */
      if ((status & 0xFFFF) != 0)
       {
         printf("   ABORTING ON CAMAC ERROR - F(9)A(2) - STATUS%9X\n",status);
         return;
       }
      cam_mode = 1;
      cam_count = 256;
      cama = 2;
      camf = 2;
      camacio_(&cam_mode,c,n,&cama,&camf,iarray,&cam_count,&status);
                                                             /* READ MEM  */
      if((status & 0xFFFF) != 0)
       {
         printf("   ABORTING ON CAMAC ERROR - F(2)A(2) - STATUS%9X\n",status);
         return;
       }

      for (l=0; l <= 255; l++)
       {
        addr=iarray[l]/256;
        if (addr != l)
             printf("ADDRESS ERROR: EXPECTED ADDR=%3i READ ADDR=%3i\n",l,addr);
        dat=(iarray[l] & 0xFF);
        if (dat != array[l])
                 printf("DATA COMPARE ERROR: ADDR=%3i WROTE %2X READ %4X\n",
                                                      addr,array[l],iarray[l]);
       }

      if (func == 3) gen(array,&func,&erro);
      if (erro != 0) return;
      cam_mode = 0;
      cam_count = 0;
      cama = 3;
      camf = 9;
      camacio_(&cam_mode,c,n,&cama,&camf,&cam_count,&cam_count,&status);
      if ((status & 0xFFFF) != 0)
       {
         printf("   ABORTING ON CAMAC ERROR - F(9)A(3) - STATUS%9X\n");
         return;
       }
      cam_mode = 1;
      cam_count = 256;
      cama = 1;
      camf = 18;
      camacio_(&cam_mode,c,n,&cama,&camf,array,&cam_count,&status);
                                                              /* LOAD MEM */
      if ((status & 0xFFFF) != 0)
       {
         printf("   ABORTING ON CAMAC ERROR - F(18)A(1) - STATUS%9X\n",status);
         return;
       }
      cam_mode = 0;
      cam_count = 0;
      cama = 3;
      camf = 9;
      camacio_(&cam_mode,c,n,&cama,&camf,&cam_count,&cam_count,&status);
                                                            /* CLEAR MEM ADDR */
      if ((status & 0xffff) != 0)
       {
         printf("   ABORTING ON CAMAC ERROR - F(9)A(3) - STATUS&9x\n",status);
         return;
       }
      cam_mode = 1;
      cam_count = 256;
      cama = 3;
      camf = 2;
      camacio_(&cam_mode,c,n,&cama,&camf,iarray,&cam_count,&status);
                                                            /* READ MEM */
      if ((status & 0xFFFF) != 0)
       {
         printf("   ABORTING ON CAMAC ERROR - F(2)A(3) - STATUS%9X\n",status);
         return;
       }

      for (l=0; l <= 255; l++)
       {
        addr=iarray[l]/256;
        if (addr != l)
             printf("ADDRESS ERROR: EXPECTED ADDR=%3i READ ADDR=%3i\n",l,addr);
        dat=(iarray[l] & 0xFF);
        if (dat != array[l])
                 printf("DATA COMPARE ERROR: ADDR=%3i WROTE %2X READ %4X\n",
                                                      addr,array[l],iarray[l]);
       }
      printf("*** LeCroy 4508 loaded and verified ***\n");

}
/******************************************************************************
******************************************************************************/
void gen(int *a,int *func,int *erro)
{

static int k,l,m;
static int in[8],out[8];

      for (m=0; m <= 255; m++)
       {
        l=1;
        for(k=0; k < 8; k++)
         {
          in[k]=(m & l)/l;
          l=l*2;
         }

        if (*func == 0) lige(in,out);
        else if (*func == 1) lbgo(in,out);
        else if (*func == 2) lanti(in,out);
        else
         {
          printf(" FUNCTION SPECIFICATION ERROR 'GEN'\n");
          return;
         }

        l=1;
        a[m]=0;
        for (k=0; k < 8; k++)
         {
          a[m]=a[m] + (out[k] & 1)*l;
          l=l*2;
         }
       }
}
/******************************************************************************
******************************************************************************/
void lanti(int *in,int *out)
{

      out[0]=(!(in[0]) & in[4]);
      out[1]=(!(in[1]) & in[5]);
      out[2]=(!(in[2]) & in[6]);
      out[3]=(!(in[3]) & in[7]);
      out[4]=(in[0] & in[4]);
      out[5]=(in[1] & in[5]);
      out[6]=(in[2] & in[6]);
      out[7]=(in[3] & in[7]);
}
/******************************************************************************
******************************************************************************/
void lbgo(int *in,int *out)
{

      out[0]=in[0];
      out[1]=in[1];
      out[2]=in[2];
      out[3]=in[3];
      out[4]=0;
      out[5]=0;
      out[6]=0;
      out[7]=0;
}
/******************************************************************************
******************************************************************************/
void lige(int *in,int *out)
{

      out[0]=in[4];
      out[1]=in[5];
      out[2]=in[6];
      out[3]=in[7];
      out[4]=0;
      out[5]=0;
      out[6]=0;
      out[7]=0;
}
