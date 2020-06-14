C$PROG EXABSTAT  - Returns EXABYTE status parameters
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
C     Routine which displays/logs, returns EXABYTE status information
C     via Charles Thomas's C routines
C     ------------------------------------------------------------------
C     MODE = 1 says display/log EXABYTE status information
C     MODE = 2 says return      EXABYTE status insormation
C     LCT  = Tape Channel#
C     TMBU = Total Megabytes Used (returned)
C     TMBR = Total Megabytes Left (returned)
C     RATE = #errors/megabyte
C     NER  = Number of errors
C     ------------------------------------------------------------------
C
      SUBROUTINE EXABSTAT(MODE,LCT,TMBU,TMBR,RATE,NER,IERR)
C
      REAL*8 TMB,RMB,ERATE,UMB
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      DATA NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(NCALL.GT.0) GO TO 100
C
      CALL CAM_OPEN(LUC)                   !OPEN CAM DEVICE FOR DIRECT
C                                          !SCSI COMMANDS
C
      IF(LUC.LE.0) GO TO 500               !TST FOR ERROR
C
      NCALL=1
      KERR=0
C
  100 IF(KERR.NE.0) GO TO 500
C
      CALL MT_INFO(LCT,ICTLR,IDN,IERR)     !GET SCSI CONTROLLER & ID
C                                          !INFO FOR EXABYTE TAPE
C
      IF(IERR.NE.0) GO TO 520
C
      CALL MT_CAP(LUC,ICTLR,IDN,KBMAX,IERR)      !GET TAPE CAPACITY
C
      IF(IERR.NE.0) GO TO 530
C
      NERR=0
      NLFT=0
C
      CALL MT_STX(LUC,ICTLR,IDN,NLFT,NERR,IERR)  !GET USE & ERROR 
C                                                !INFORMATION
C
      TMB=0.001D0*DFLOAT(KBMAX)
      RMB=0.001D0*DFLOAT(NLFT)
Crlv      ERATE=DFLOAT(NERR)/(TMB-RMB-1.279D0)
      if (tmb-rmb .gt. 0.0D0) then
         ERATE=DFLOAT(NERR)/(TMB-RMB)
      else
         erate=0.0
      endif
Crlv      UMB=TMB-RMB-1.279D0
      UMB=TMB-RMB
C
      IF(MODE.EQ.2) THEN
                    TMBU=UMB
                    TMBR=RMB
                    RATE=ERATE
                    NER=NERR
                    RETURN
                    ENDIF
C
      WRITE(CMSSG,110)UMB,RMB,NERR,ERATE
  110 FORMAT('MB-used, MB-left, #-ERR, ERR/MB =',2F10.3,I8,F8.2)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     ------------------------------------------------------------------
C     SET UP AND SEND ERROR MESSAGES
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('FAILED TO OPEM CAM DEVICE - STATUS INFO UNAVAILABLE!')
      KERR=1
      GO TO 600
C
  520 WRITE(CMSSG,525)
  525 FORMAT('Error getting MT_INFO. -  Command ignored!')
      GO TO 600
C
  530 WRITE(CMSSG,535)IERR
  535 FORMAT(' Error getting tape capacity info.  Status= ',I4)
      GO TO 600
C
  600 CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C
      IERR=1
      IF(MODE.NE.2) RETURN
C
      TMBU=0.0
      TMBR=0.0
      RATE=0.0
      NER=0
      RETURN
      END
