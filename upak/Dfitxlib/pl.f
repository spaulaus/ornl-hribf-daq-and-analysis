C$PROG PL        - Returns Legendre polynomial term values
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/30/2000
C     ******************************************************************
C
      REAL*8 FUNCTION PL(N,X)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ------------------------------------------------------------------
C     RETURNS LEGENDRE POLY TERM VALUE AS FUNCTION OF (N,X)
C     ------------------------------------------------------------------
C
      JGO=N+1
      GO TO (1,2,3,4,5,6,7),JGO
    1 PL=1.0
      RETURN
    2 PL=X
      RETURN
    3 PL=1.5*X*X-0.5
      RETURN
    4 PL=2.5*X**3-1.5*X
      RETURN
    5 PL=4.375*X**4-3.75*X*X+0.375
      RETURN
    6 PL=7.875*X**5-8.75*X**3+1.875*X
      RETURN
    7 PL=14.4375*X**6-19.6875*X**4+6.5625*X**2-0.3125
      RETURN
      END
