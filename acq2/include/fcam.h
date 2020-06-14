/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1998
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
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dvme3/fcam.h
*
*    Description:  
*                  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   10/22/98    MCSQ         
*
*****************************************************************************/

#ifndef  FCAM_H_
#define  FCAM_H_

/*  Register access structure                                                 */

struct FCAM  {
     union  {
              volatile int cnaf;
              volatile int l;
              volatile unsigned short s;
        } dat;
      int cmd;
};

#define RS_FIFO  1          /* Reset read fifo buffer               */
#define RS_CAM   2          /* Reset CAMAC AUX controller           */

/****************************************************************************/

/*  c = crate number
    n = station number
    a = subaddress
    f = function
*/
#define CNAF(c,n,a,f)  ((c<<24) | (n<<9) | (a<<5) | f)
#define CNAF24(c,n,a,f)  ((c<<24) | (n<<9) | (a<<5) | f | 0x4000)
#define QREPEAT(c,n,a,f)  ((c<<24) | (n<<9) | (a<<5) | f | 0x8000)
#define QREPEAT24(c,n,a,f)  ((c<<24) | (n<<9) | (a<<5) | f | 0xc000)


#define QMSK  0x80000000   /* CAMAC Q response. 0 means Q = 1      */
#define XMSK  0x40000000   /* CAMAC X response. 0 means X = 1      */
#define TMSK  0x20000000   /* Read timeout.  1 means timeout       */

/****************************************************************************/

#endif      /* end  FCAM_H_   */
