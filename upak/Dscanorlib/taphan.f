C$PROG TAPHAN    - Does tape control functions
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 07/19/99
C     ******************************************************************
C
      SUBROUTINE TAPHAN(KMD,NV,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      CHARACTER*4  KMD
C
      INTEGER*4    NV,IERR,C,P1,I
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0                                    !RESET ERROR FLAG
      C=LUC(1)                                  !CHANNEL NUMBER
C
      P1=NV                                     !# OF TIMES TO DO IT
      IF(P1.LT.1) P1=1                          !DO IT AT LEAST ONCE

      IF(KMD.EQ.'FR  ') CALL MT_FR(C,P1,IERR)   !FORWARD RECORD
      IF(KMD.EQ.'BR  ') CALL MT_BR(C,P1,IERR)   !BACKUP  RECORD
      IF(KMD.EQ.'FF  ') GO TO 100               !FORWARD FILE
      IF(KMD.EQ.'BF  ') GO TO 120               !BACKUP  FILE
      IF(KMD.EQ.'REW ') CALL MT_REW(C,IERR)     !REWIND
      IF(KMD.EQ.'REWU') CALL MT_REWUL(C,IERR)   !REWIND AND UNLOAD
C
      GO TO 150
C
  100 DO 110 I=1,P1
      CALL MT_FF(C,1,IERR)
      IF(IERR.NE.0)      GO TO 150
      IF(MSGF.NE.'    ') GO TO 150
  110 CONTINUE
      GO TO 150
C
C
  120 DO 130 I=1,P1
      CALL MT_BF(C,1,IERR)
      IF(IERR.NE.0)      GO TO 150
      IF(MSGF.NE.'    ') GO TO 150
  130 CONTINUE
      GO TO 150
C
C
  150 IF(IERR.EQ.0) RETURN
C
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
      IF(IERR.LT.0) THEN
      WRITE(CMSSG,145)IERR
  145 FORMAT('TAPE HANDLER ERROR - IERR =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 200
      ENDIF
C
      CALL IOSERR(IERR)
C
  200 IERR=1
      RETURN
      END
