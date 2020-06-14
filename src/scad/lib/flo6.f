C$PROG FLO6      - Converts floating# to 6-digits ASCII (FLOAT or INT)
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLO6(X,JV)
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
C     CONVERTS FLOATING# (X) TO 6-DIGIT ASCII FLOAT OR INTEGER (JV)
C     ****************************************************************
C
      IF(X.EQ.0.0)      GO TO 50
      IF(X.LT.0.1)      GO TO 10
      IF(X.LT.9.999)    GO TO 20
      IF(X.LT.99.99)    GO TO 30
      IF(X.LT.999.9)    GO TO 40
      IF(X.LT.99999.0)  GO TO 50
C
   10 WRITE(JV,15)X
   15 FORMAT(3PE9.2,'   ')
      CALL ISBYTE(X20,JV,4)
      CALL ISBYTE(X20,JV,5)
      CALL ISBYTE(X20,JV,7)
      CALL SQUEZL(JV,2,9)
      RETURN
C
   20 WRITE(JV,25)X
   25 FORMAT(F6.3,'      ')
      RETURN
C
   30 WRITE(JV,35)X
   35 FORMAT(F6.2,'      ')
      RETURN
C
   40 WRITE(JV,45)X
   45 FORMAT(F6.1,'      ')
      RETURN
C
   50 IV=X
      WRITE(JV,55)IV
   55 FORMAT(I6,  '      ')
      RETURN
      END
