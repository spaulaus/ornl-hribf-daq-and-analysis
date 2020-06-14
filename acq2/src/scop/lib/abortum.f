C$PROG ABORTUM   - Closes EPICS connection and EXITs
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 03/16/2000
C     ******************************************************************
C
      SUBROUTINE ABORTUM
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/EP01/ BLNAME,EPOPEN
      CHARACTER*5  BLNAME,EPOPEN
C     ------------------------------------------------------------------
      CHARACTER*128 ERRMSG
C
      INTEGER*4     IERR
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(EPOPEN.EQ.'YES') CALL FCLOSE_EPICS(IERR,ERRMSG)
C
      IF(IERR.NE.0) WRITE(6,10)ERRMSG
   10 FORMAT(' ',A)
C
      CALL EXIT(0)
C
      END
