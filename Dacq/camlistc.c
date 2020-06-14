/*****************************************************************************
*  Replacements for routines camlist_ and camacio_
*
*    These replace routines in vmelib.a for testing m_setup.c
*
*    Link gcc with camlistc.o instead of vmelib.a
*
*****************************************************************************/
/*****************************************************************************
*    CAMAC List Processor
*  Call:    c      - Crate number array
*           n      - Station number array
*           a      - Subaddress array
*           f      - Function code array
*           m      - Data mode: 0 = 16-bit, non-zero 24-bit(integer*4)
*           data   - I/O data array.
*           len    - Number of data to transfer.
*           stat   - Returned status array(i.e. X, Q etc).
*****************************************************************************/
void camlist_(int *c,int *n,int *a,int *f,int *m,void *data,
              int *len,int *stat)
{
   unsigned short *dat16,*dat16s;
            int   *dat24,*dat24s;
   int  i,j,cnt,estatus;
   int *cc,*nn,*aa,*ff;
   int *fs,fsave;
   int *status,*statusb;
   int Data;

   cc = c;
   nn = n;
   aa = a;
   ff = fs = f;
   dat24 = dat24s = data;
   dat16 = dat16s = data;
   status = statusb = stat;
   i = 0;
   cnt = *len;
   while (cnt > 0)
    {
      Data = 0;
      *status = 0;
      fsave = *ff;
      printf("CAMLIST: C,N,A,F,DATA,STAT: %i %i %i %i",*cc++,*nn++,*aa++,*ff++);
      if ((fsave & 0x18) == 0x10)
       {
/*
*   CAMAC write operations - F(16) thru F(23)
*/
         if (*m) Data = *dat24;
         else  Data = *dat16;
       }
      else if ((fsave & 0x18) == 0)
       {
/*
*   CAMAC read operations - F(0) thru F(15)
*/
         if(*m) *dat24 = 0;
         else  *dat16 = 0;
         Data = 0;
       }
     if (*status == 0) *status = 0;
     printf("  %X  %i\n",Data,*status);
     status++;
     dat16++;
     dat24++;
     i++;
     cnt--;
    }
   return;
}

void camacio_(int *mode,int *c,int *n,int *a,int *f,void *data,
                                                         int *nwds, int *istat)
{
   unsigned short *dat16;
            int   *dat24;
  int tmo = 5;
  int cnt;
  char ipb[4];
  int i,lu,stat,one = 1;

   dat24  = data;
   dat16  = data;
  cnt = *nwds;
  if (*f >= 8 && *f < 16) cnt = 1;
  if (*f >= 24) cnt = 1;
  for (i=0; i < cnt; i++)
   {
    if (*mode) camlist_(c,n,a,f,mode,dat24++,&one,istat);
    else camlist_(c,n,a,f,mode,dat16++,&one,istat);
   }
}

