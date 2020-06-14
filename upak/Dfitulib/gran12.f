C$PROG GRAN12    - Generates 12-point Gaussian random number
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/30/2000
C     ******************************************************************
C
      FUNCTION GRAN12(ISEED)
C
      SUM=0.0
C
      DO 10 I=1,12
      SUM=SUM+RAN(ISEED)
   10 CONTINUE
      GRAN12=2.0*SUM-12.0
      RETURN
      END
