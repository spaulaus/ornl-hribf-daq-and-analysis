C$PROG DDET_FUN  - REAL*8 determinant of 3x3 matrix
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      REAL*8 FUNCTION DDET_FUN(A1,A2,A3,B1,B2,B3,C1,C2,C3)
C
      IMPLICIT REAL*8 (A-Z)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DDET_FUN=A1*(B2*C3-B3*C2)+A2*(B3*C1-B1*C3)+A3*(B1*C2-B2*C1)
C
      RETURN
C
      END
