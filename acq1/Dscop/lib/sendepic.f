C$PROG SENDEPIC  - Sends rate data to EPICS server
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 05/03/2004
C     ******************************************************************
C
      SUBROUTINE SENDEPIC(RATE,NRATE,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ==================================================================
C
      REAL*4      RATE(*)
C
      INTEGER*4   NRATE,IERR,CNTDOWN,I
C
      DATA        CNTDOWN/1/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      CNTDOWN=CNTDOWN-1
C
      IF(CNTDOWN.GT.0) RETURN
C
CX    WRITE(6,10)(RATE(I),I=1,NRATE)
CX 10 FORMAT(' Rates =',4F10.0/)
C
      CNTDOWN=5
C
      RETURN
      END
C
