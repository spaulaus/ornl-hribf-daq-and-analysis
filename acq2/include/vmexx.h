/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                              Copyright(C) 2001
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
*    Environment:  VME Acquisition system
*
*    File:         rtems/include/vmexx.h
*
*    Description:  Data structures for the VME Acq module control functions.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/ 8/01    MCSQ
*
*   10/31/01    MCSQ        Add mode param for TDCs.
*    8/01/15    RLV         Adding the MyRIAD module at ANL/DGS
*****************************************************************************/
#ifndef  VMEXX_H_
#define  VMEXX_H_
#include <inttypes.h>

/*
*   Software Commands and data structures
*   For reasons I do not recall, MCSQ used byte swap safe numbers
*/
#define  CAEN775       0x1010  /* CAEN TDC modules                        */
#define  CAEN785       0x1111  /* CAEN ADC modules                        */
#define  CAEN792       0x0101  /* CAEN QDC modules                        */

#define  SIS3820MOD    0x2222  /* SIS3820 VME Scaler                      */
#define  MYRIADMOD     0x2020  /* MyRIAD timestamp and trigger module          */

struct  caen_ctl {
  short func;      /* function code - CAEN775, CAEN785, CAEN792 */
  char  rpystat;   /* reply status code                         */
  char  cvt_num;   /* ADC or TDC number, 1 thru 12              */
  char  rw;        /* Read/write flag, 0 = read, nonzero = write */
  char  range;     /* TDC only                                  */
  short data[32];  /* Thresholds                                */
  char  mode;      /* TDC mode. 0 = common start                */
                   /*           1 = common stop                 */
};

struct  sis_ctl {
  short func;      /* function code - SIS3820MOD                */
  char  rpystat;   /* reply status code                         */
  char  sca_num;   /* Scaler number                             */
  char  code;      /* execution code                            */
                   /* 0 = read, 1 = clear, 2 = enable counting  */
                   /* 3 = disable counting                      */
  char  dum[3];
  uint32_t  data[64];  /* Counter data - last word has higher bits of 1&17   */
  //  unsigned long  data[32];  /* Counter data                              */
};

struct  myriad_ctrl {
  short func;      /* function code - MYRIADMOD                 */
  char  rpystat;   /* reply status code                         */
  char  code;      /* execution code                            */
                   /* 0 = read, 1 = clear, 2 = enable counting  */
                   /* 3 = disable counting                      */
  char  dum[4];
  uint32_t  data[32];  /* Counter data                              */
};

struct  vmeio_stat {
  short func;
  char  rpystat;   /* reply status code                         */
};

union Cmd {
  struct caen_ctl conv;
  struct sis_ctl  sca;
  struct vmeio_stat reply;
};

/*
*   Error codes
*/
#define  VME_ERR_FUNC            -1

#define  CAEN_ERR_NONEXIST   -2
#define  SCA_ERR_NONEXIST    -2
#define  MYR_ERR_NONEXIST    -2

#endif     /* end VMEXX_H_    */
