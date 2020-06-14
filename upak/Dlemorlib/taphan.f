C$PROG TAPHAN    - Does control functions for Input & Output tapes
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE TAPHAN(C,KOM,NV,STAT)
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
      CHARACTER*4  KOM,KMD,STAT
C
      INTEGER*4    C,NV,IERR,P1,I
C
      SAVE
C
C     ------------------------------------------------------------------
C
      STAT='GOOD'
      IERR=0                              !RESET ERROR FLAG
      KMD=KOM
      KMD(3:3)=' '                        !ZOT 3rd COMMAND byte
C
      P1=NV                               !# OF TIMES TO DO IT
      IF(P1.LT.1) P1=1                    !DO IT AT LEAST ONCE
C
      IF(KMD.EQ.'FR  ') CALL MT_FR(C,P1,IERR) !FORWARD RECORD
C
      IF(KMD.EQ.'BR  ') CALL MT_BR(C,P1,IERR) !BACKUP  RECORD
C
      IF(KMD.EQ.'FF  ') GO TO 100             !FORWARD FILE
C
      IF(KMD.EQ.'BF  ') GO TO 120             !BACKUP  FILE
C
      IF(KMD.EQ.'RW  ') CALL MT_REW(C,IERR)   !REWIND
C
      IF(KMD.EQ.'RU  ') CALL MT_REWUL(C,IERR) !REWIND AND UNLOAD
C
      IF(KMD.EQ.'EO  ') CALL MT_WEOF(C,IERR)  !WRITE FILE-MARK
C
      GO TO 150
C
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
  200 STAT='ERR '
      RETURN
      END
