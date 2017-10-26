C$PROG FLO10     - Converts floating# to 10 digit ASCII (FOAT or INT)
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLO10(X,JV)
C
      CHARACTER*12  JV
C
      SAVE
C
C     ****************************************************************
C     CONVERTS FLOATING# (X) TO 10-DIGIT ASCII FLOAT OR INTEGER (JV)
C     ****************************************************************
C
      IF(X.EQ.0.0)      GO TO 40
      IF(X.LT.0.01)     GO TO 10
      IF(X.LT.999.998)  GO TO 20
      IF(X.LT.999998.0) GO TO 40
C
   10 WRITE(JV,15)X
   15 FORMAT(5PE12.4)
      RETURN
C
   20 WRITE(JV,25)X
   25 FORMAT(F12.4)
      RETURN
C
   40 IV=X
      WRITE(JV,45)IV
   45 FORMAT(I12)
      RETURN
      END
