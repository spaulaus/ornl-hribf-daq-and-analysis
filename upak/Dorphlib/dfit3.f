C$PROG DFIT3     - Fits 3 X,Y points to Y = A + B*X + C*X*X (REAL*8)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      REAL*8 FUNCTION DFIT3(X1,X2,X3,Y1,Y2,Y3,Q)
C
      IMPLICIT REAL*8 (A-Z)
C
      SAVE
C
C     ------------------------------------------------------------------
C     FITS THE THREE POINTS - (X1,Y1), (X2,Y2), (X3,Y3) TO
C     FUNCTION OF THE FORM  - Y=A+B*X+C*X**2
C     AND RETURNS THE VALUE OF "Y" AT X=Q
C     ------------------------------------------------------------------
C
      S1=X1*X1
      S2=X2*X2
      S3=X3*X3
      UN=1.0D0
      D=DDET_FUN(UN,X1,S1,UN,X2,S2,UN,X3,S3)
      A=DDET_FUN(Y1,X1,S1,Y2,X2,S2,Y3,X3,S3)
      B=DDET_FUN(UN,Y1,S1,UN,Y2,S2,UN,Y3,S3)
      C=DDET_FUN(UN,X1,Y1,UN,X2,Y2,UN,X3,Y3)
      DFIT3=(A+B*Q+C*Q*Q)/D
      RETURN
      END
