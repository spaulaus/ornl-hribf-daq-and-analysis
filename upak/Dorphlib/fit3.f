C$PROG FIT3      - Fits 3 X,Y points to Y = A + B*X + C*X*X (REAL*4)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      FUNCTION FIT3(X1,X2,X3,Y1,Y2,Y3,Q)
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
      D=DET_FUN(1.,X1,S1,1.,X2,S2,1.,X3,S3)
      A=DET_FUN(Y1,X1,S1,Y2,X2,S2,Y3,X3,S3)
      B=DET_FUN(1.,Y1,S1,1.,Y2,S2,1.,Y3,S3)
      C=DET_FUN(1.,X1,Y1,1.,X2,Y2,1.,X3,Y3)
      FIT3=(A+B*Q+C*Q*Q)/D
      RETURN
      END
