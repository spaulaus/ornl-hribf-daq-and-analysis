C$PROG NEAR2     - Returns power-of-2 value .GE. arg
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/17/99
C     ******************************************************************
C
      INTEGER*4 FUNCTION NEAR2(I)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4   I,J,NINT
C
      REAL*4      X,Y,Z,LOG
C     ------------------------------------------------------------------
C
      NEAR2=I
      IF(I.LE.0)RETURN
      Z=I
      X=LOG(Z)
      Y=X/LOG(2.)
      J=NINT(Y)
      NEAR2=2**J
      RETURN
      END
