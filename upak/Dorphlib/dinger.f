C$PROG DINGER    - Dings the dinger N-times
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 12/17/2002
C     ******************************************************************
C
      SUBROUTINE DINGER(NDO)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4   NDO,N,BELL,ISTAT,I
C
      DATA        BELL/Z'07202020'/
C
      SAVE
C     ------------------------------------------------------------------
C
      N=NDO
C
      IF(N.GT.10) N=10
C
      DO 20 I=1,N
C
      CALL WAIT(200,1,ISTAT)
C
      WRITE(6,10)BELL
C
   10 FORMAT(A4,$)
C
   20 CONTINUE
C
      WRITE(6,25)
C
   25 FORMAT(1H )
C
      RETURN
      END

