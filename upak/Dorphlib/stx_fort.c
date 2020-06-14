/* Fortram callable routines to access EXABYTE internal status via CAM */
/*   C.N.T. 01/93  */
/*   C.N.T. 10/94 ported to OSF  */
/*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    CAM_OPEN(clu)
Open CAM device for direct access to EXABYTE functions
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    CAM_CLOSE(clu)
Close CAM device.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    MT_INFO(tlu,ctlr,id,ierr)
Get EXABYTE interface info
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    MT_CAP(clu,ctlr,id,size,ierr)
Get EXABYTE total capacity
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    MT_STX(clu,ctlr,id,rem,nerr,ierr)
Get EXABYTE internal info.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*/

#include <sys/types.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <sys/buf.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <errno.h>
#include <io/common/devgetinfo.h>
#include <io/common/iotypes.h>
#include <io/cam/cam.h>
#include <io/cam/dec_cam.h>
#include <io/cam/scsi_status.h>
#include <io/cam/scsi_all.h>
#include <io/cam/uagt.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <memory.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include "stx.h"
#define MODE_SENSE_LEN	16
#define REQ_SENSE_LEN   26

void clear_mem();

/***************************************
    Fortran call sequence:
    CALL CAM_OPEN(clu)
    get CAM logical unit for subsequent CAM operations
*/
void cam_open_(int *clu)
{
    *clu = open("/dev/cam",O_RDWR,0);
}
/***************************************
    Fortran call sequence:
    CALL CAM_CLOSE(clu)
*/
void cam_close_(int *clu)
{
    close(*clu);
}      
/***************************************
   Fortran call sequence:
   CALL MT_INFO(tlu,ctlr,id,status)
   get hardware info for tape drive
*/
void mt_info_(int *tlu, int *ctlr,int *id, int *ierr)
{
/*
	tlu   file descriptor for tape drive
	ctlr  returned SCSI controller number
	id    returned SCSI device ID number
        ierr  returned status (0 = good)
*/
    device_info_t *devinfop;  /* pointer to device information */

/*    allocate storage for the device information returned by ioctl */
    devinfop = (device_info_t *)malloc(sizeof(device_info_t));
    if (!devinfop) {
       *ierr = 1;
       return;
    }

/*    use ioctl to get the information                              */
    if (ioctl(*tlu,DEVGETINFO,(char *)devinfop) < 0) 
    {
       *ierr = 1;
    }
    else
    {
       *ierr = 0;
       *ctlr = devinfop->v1.businfo.bus.scsi.bus_num;
       *id   = devinfop->v1.businfo.bus.scsi.tgt_id;
    }
    free(devinfop);
}
/***************************************
    Fortran call sequence:
    CALL MT_CAP(clu,ctlr,id,size,ierr)
    get total capacity of tape
*/
void mt_cap_(int *clu,int *ctlr,int *id,int *size,int *ierr)
{
/*     clu    file descripter for CAM device
       ctlr   SCSI controller number
       id     SCSI device ID number
       size   returned capacity of tape
       ierr   returned error status (0 = good)
*/

   u_char mbuf[MODE_SENSE_LEN];

   UAGT_CAM_CCB mua_ccb;		/* local uagt structure */
   CCB_SCSIIO mccb;			/* local CCB */

   ALL_MODE_SENSE_CDB6 *msns;	/* pointer for CDB's */
   ALL_MODE_SENSE_DATA *mptr;   /* pointer for SENSE data */

      /* Had to clear out the structs to prevent hangs! */
	clear_mem(&mua_ccb, sizeof(mua_ccb));
	clear_mem(&mccb, sizeof(mccb));

/* +++++++++++++ set up MODE SENSE operation +++++++++++++++++ */
    /* Set up the CAM header for the XPT_SCSI_IO function */
    mccb.cam_ch.my_addr = (struct ccb_header *)&mccb;
    mccb.cam_ch.cam_ccb_len = sizeof(CCB_SCSIIO);
    mccb.cam_ch.cam_func_code = XPT_SCSI_IO;
    mccb.cam_ch.cam_path_id = *ctlr;
    mccb.cam_ch.cam_target_id = *id; 
    mccb.cam_ch.cam_target_lun = 0;
    mccb.cam_ch.cam_flags = CAM_DIR_IN | CAM_DIS_AUTOSENSE |
                            CAM_ENG_SYNC;

    mccb.cam_data_ptr = &mbuf[0];
    mccb.cam_dxfer_len = MODE_SENSE_LEN;
    mccb.cam_timeout = CAM_TIME_DEFAULT;
    mccb.cam_cdb_len = sizeof( ALL_MODE_SENSE_CDB6 );

    msns = (ALL_MODE_SENSE_CDB6 *)&mccb.cam_cdb_io.cam_cdb_bytes[0];

    msns->opcode = ALL_MODE_SENSE6_OP;
    msns->dbd = 0;
    msns->lun = 0;
    msns->page_code = 0;
    msns->alloc_len = MODE_SENSE_LEN;
    msns->control = 0;

    mua_ccb.uagt_ccb = (CCB_HEADER *)&mccb;
    mua_ccb.uagt_ccblen = sizeof(CCB_SCSIIO);
    mua_ccb.uagt_buffer = &mbuf[0];
    mua_ccb.uagt_buflen = MODE_SENSE_LEN;

    mua_ccb.uagt_snsbuf = (u_char *)NULL;
    mua_ccb.uagt_snslen = 0;
    mua_ccb.uagt_cdb = (CDB_UN *)NULL;
    mua_ccb.uagt_cdblen = 0;
    mua_ccb.uagt_flags = 0;

    if (ioctl(*clu, UAGT_CAM_IO, (caddr_t)&mua_ccb) < 0)
       *ierr = 1;
    else if (mccb.cam_ch.cam_status != CAM_REQ_CMP)
       *ierr = mccb.cam_ch.cam_status;
    else {
      *ierr = 0;
      mptr = (ALL_MODE_SENSE_DATA *)mbuf;
      *size = ((mptr->nblks0)
              +(mptr->nblks1 * 0x100)
              +(mptr->nblks2 * 0x10000));
    }
}
/***************************************
    Fortran call sequence:
    CALL MT_STX(clu,ctlr,id,rem,nerr,ierr)
    get space remaining and internal error count
*/
void mt_stx_(int *clu,int *ctlr,int *id,int *rem,int *nerr,int *ierr)
{
/*     clu    file descripter for CAM device
       ctlr   scsi controller number
       id     scsi id number
       size   returned capacity of tape
       rem    returned remaining space on tape
       nerr   returned internal error count from drive
       ierr   returned status  (0 = good)
*/
   u_char rbuf[REQ_SENSE_LEN];
   UAGT_CAM_CCB rua_ccb;		/* local uagt structure */
   CCB_SCSIIO rccb;			/* local CCB */

   ALL_REQ_SENSE_CDB6  *rsns;
   ALL_REQ_SENSE_DATA  *rptr;

      /* Had to clear out the structs to prevent hangs! */
	clear_mem(&rua_ccb, sizeof(rua_ccb));
	clear_mem(&rccb, sizeof(rccb));

/* +++++++++++ set up REQUEST SENSE operation +++++++++++++++++ */
    /* Set up the CAM header for the XPT_SCSI_IO function */
	rccb.cam_ch.my_addr = (struct ccb_header *)&rccb;
	rccb.cam_ch.cam_ccb_len = sizeof(CCB_SCSIIO);
	rccb.cam_ch.cam_func_code = XPT_SCSI_IO;
	rccb.cam_ch.cam_path_id = *ctlr;
	rccb.cam_ch.cam_target_id = *id;
	rccb.cam_ch.cam_target_lun = 0;

	rccb.cam_ch.cam_flags = CAM_DIR_IN | CAM_DIS_AUTOSENSE |
                                CAM_ENG_SYNC;

	rccb.cam_data_ptr = &rbuf[0];
	rccb.cam_dxfer_len = REQ_SENSE_LEN;
	rccb.cam_timeout = CAM_TIME_DEFAULT;
	rccb.cam_cdb_len = sizeof( ALL_REQ_SENSE_CDB6 );

	rsns = (ALL_REQ_SENSE_CDB6 *)&rccb.cam_cdb_io.cam_cdb_bytes[0];

	rsns->opcode = ALL_REQ_SENSE6_OP;
	rsns->lun = 0;
	rsns->alloc_len = REQ_SENSE_LEN;
	rsns->control = 0;

	rua_ccb.uagt_ccb = (CCB_HEADER *)&rccb;
	rua_ccb.uagt_ccblen = sizeof(CCB_SCSIIO);
	rua_ccb.uagt_buffer = &rbuf[0];
	rua_ccb.uagt_buflen = REQ_SENSE_LEN;
	rua_ccb.uagt_snsbuf = (u_char *)NULL;
	rua_ccb.uagt_snslen = 0;
	rua_ccb.uagt_cdb = (CDB_UN *)NULL;
	rua_ccb.uagt_cdblen = 0;

        *rem  = 0;
        *nerr = 0;
        *ierr = 0;

	if (ioctl(*clu, UAGT_CAM_IO, (caddr_t)&rua_ccb) < 0 ) 
           *ierr = 1;
        else if (rccb.cam_ch.cam_status != CAM_REQ_CMP)
           *ierr = rccb.cam_ch.cam_status;
        else {
	  rptr = (ALL_REQ_SENSE_DATA *)rbuf;

	  *rem  = ((rptr->rblks0)
	          +(rptr->rblks1 * 0x100)
	          +(rptr->rblks2 * 0x10000)); 

          *nerr = ((rptr->err_byte0)
                  +(rptr->err_byte1 * 0x100)
	          +(rptr->err_byte2 * 0x10000)); 
        }
}
/***************************************/
void clear_mem(bp,n)    /* Clear n bytes of memory */
u_char *bp;
int    n;
{
	register i;
	register u_char *ptr;
	for(i=0,ptr=bp; i<n; i++, ptr++) *ptr = 0;
}
