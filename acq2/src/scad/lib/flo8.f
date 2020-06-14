C$PROG FLO8      - Converts floating# to 8-digits ASCII (FLOAT or INT)
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLO8(X,JV)
C
      CHARACTER*12  JV
C
      INTEGER*4     X20
C
      DATA          X20/'20'X/
C
      SAVE
C
C     ****************************************************************
C     CONVERTS FLOATING# (X) TO 8-DIGIT ASCII FLOAT OR INTEGER (JV)
C     ****************************************************************
C
      IF(X.EQ.0.0)      GO TO 40
      IF(X.LT.0.01)     GO TO 10
      IF(X.LT.99.998)   GO TO 20
      IF(X.LT.9998.0)   GO TO 30
      IF(X.LT.9999998.) GO TO 40
C
   10 WRITE(JV,15)X
   15 FORMAT(5PE11.4,' ')
      CALL ISBYTE(X20,JV,6)
      CALL ISBYTE(X20,JV,7)
      CALL ISBYTE(X20,JV,9)
      CALL SQUEZL(JV,2,11)
      RETURN
C
   20 WRITE(JV,25)X
   25 FORMAT(F8.4,'    ')
      RETURN
C
   30 WRITE(JV,35)X
   35 FORMAT(F8.2,'    ')
      RETURN
C
   40 IV=X
      WRITE(JV,45)IV
   45 FORMAT(I8,  '    ')
      RETURN
      END
