C$PROG ALARMTST  - Tests ALARM function
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/17/2004
C     ******************************************************************
C
      SUBROUTINE ALARMTST
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SD15/ LUAL,THUSH,TNOW,ALFLG
      INTEGER*4    LUAL,THUSH,TNOW
      CHARACTER*4                  ALFLG
C     ------------------------------------------------------------------
      INTEGER*4    SEC70,STAT,N
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      DO 100 N=1,3
C
      CALL WAIT(5000,1,STAT)
C
      CALL SECSENS70(SEC70)             !Get current epoch time
C
      WRITE(LUAL,REC=1)SEC70            !Record time
C
      CALL FLUSH(LUAL)                  !Force output to disk
C
  100 CONTINUE
C
      RETURN
C
      END
