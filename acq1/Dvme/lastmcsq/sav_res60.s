*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1997
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
*    Environment:  Force CPU-60 and VMEPROM
*
*    File:         /usr/users/mcsq/vme/sav_res.s
*
*    Description:  Routines to save battery backed RAM in FLASH EEPROM
*                  and restore battery backed RAM from FLASH EEPROM.
*
*                  Two routines are normally stored in Battery backed RAM:
*                    1)  vmex_boot.s - powerup bootstrap loader
*                    2)  vme_listen  - displays received ethernet packets.
*
*                  For backup, a copy of these are stored in the onboard
*                  FLASH EEPROM at location 0xFFC80000.  These routines
*                  help save a copy of battery backed RAM in the FLASH
*                  EEPROM and restore the battery backed RAM from the
*                  FLASH EEPROM.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    3/20/93   MCSQ         Original
*
*    3/ 7/97   MCSQ         Version for Force CPU-60
*******************************************************************************
*
*
		ORG	$80000
*
*   This routine moves a copy of battery backed RAM to DRAM.  To save
*   in FLASH EEPROM, you MUST execute a commands to VMEPROM to erase and write
*   the FLASH EEPROM.  The necessary command lines is:
*
*   ? FERASE USER_FLASH
*
*   ? FPROG USER_FLASH,$100000
*
SAVE:
		MOVE.L	#RAM,A0		;Get RAM buffer address
		MOVE.L	#$40000+RESTORE-RAM,D0	;Buffer length
		MOVEQ	#-1,D1
SAVE1:
		MOVE.B	D1,(A0)+	;Initialize RAM buffer
		SUBQ.L	#1,D0
		BNE	SAVE1
		MOVE.L	#$FFC08000,A1	;Start address of boot prog in SRAM
		MOVE.L	#RESTORE+$1000,A0
		MOVE.L	#$18000,D0
SAVE2:
		MOVE.B	(A1)+,(A0)+
		SUBQ.L	#1,D0
		BNE	SAVE2
		TRAP	#0
*
		ORG	$100000
*
*   This routine copies FLASH EEPROM to the battery backed RAM
*
RESTORE:
		MOVE.L	#$FFC81000,A0
		MOVE.L	#$FFC08000,A1
		MOVE.L	#$18000,D0
RESTORE1:
		MOVE.B	(A0)+,(A1)+
		SUBQ.L	#1,D0
		BNE	RESTORE1
		TRAP	#0
*
RAM:
*
		END
