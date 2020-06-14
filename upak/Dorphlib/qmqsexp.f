C$PROG QMQSEXP   - Returns DEXP(-X*X) where X is the argument
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      REAL*8 FUNCTION QMQSEXP(X)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF (DABS(X).GE.10.) THEN
      QMQSEXP=0.
      RETURN
      END IF
      QMQSEXP=DEXP(-X*X)
      RETURN
      END
