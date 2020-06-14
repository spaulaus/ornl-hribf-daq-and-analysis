C$PROG LOGB2     - Returns log-base-2 of integer argument
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION LOGB2(IA)
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO CALCULATE LOG OF A NUMBER TO BASE-2
C     ------------------------------------------------------------------
C
      DO 10 I=1,28
      NT=2**(I-1)
      IF(NT.EQ.IA) GO TO 20
      IF(NT.GT.IA) GO TO 15
   10 CONTINUE
   15 I=0
   20 LOGB2=I-1
      RETURN
      END
