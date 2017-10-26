C$PROG FLO12     - Converts floating# to 12 digit ASCII FLOAT
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLO12(X,JV)
C
      CHARACTER*12  JV
C
      SAVE
C
C     ****************************************************************
C     CONVERTS FLOATING# (X) TO 12-DIGIT ASCII FLOAT
C     ****************************************************************
C
   20 WRITE(JV,25)X
   25 FORMAT(F12.3)
      RETURN
C
      END
