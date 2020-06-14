/****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1993
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
*    File:         /usr/users/mcsq/Dacq/cam_fast.for
*
*    Description:  Status return codes from CAMAC and FASTBUS routines
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/23/93    MCSQ         
*****************************************************************************/
/*
*  All codes are possible for CAMAC I/O.  For FASTBUS I/O, only
*  TIMEOUT and ILL_ADDRESS are possible.
*/
      #define NOQ  1                  /* X = 1, Q = 0                */
      #define NOX  2                  /* X = 0, Q = 1                */
      #define NOX_NOQ  3              /* X = 0, Q = 0                */
      #define NOQ_NOX  3              /* X = 0, Q = 0                */
      #define TIMEOUT  128            /* Timeout                     */
      #define ILL_CRATE  0xc004       /*Illegal CAMAC crate number   */
      #define ILL_MODULE  0xc005      /*Illegal CAMAC module number  */
      #define ILL_ADDRESS  0xc006     /*Illegal CAMAC subaddress     */
      #define ILL_FUNCTION  0xc009    /*Illegal CAMAC function code  */
      #define CRATE_OFF_LINE  0xa000  /*CAMAC crate off line         */
