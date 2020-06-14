C$PROG REDMILH   - Reads one record from the mil-file -HALF-WD
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/01/98
C     ******************************************************************
C
      SUBROUTINE REDMILH(IREC,BUF)
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
      INTEGER*4    IREC,IOS
C
      INTEGER*2    BUF(128)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      READ(LUC(10),REC=IREC,IOSTAT=IOS)BUF
      IF(IOS.NE.0) GO TO 20
      RETURN
C
   20 WRITE(CMSSG,30)IREC,IOS
   30 FORMAT('Error reading in mil-file - REC#,IOS =',2I8)
      CALL MESSLOG(LOGUT,LOGUP)
      STOP
      END
