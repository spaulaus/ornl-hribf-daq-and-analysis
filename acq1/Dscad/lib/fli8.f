C$PROG FLI8      - Converts INTEGER value to 8-character ASCII
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLI8(IV,JV)
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
C     CONVERTS INTEGER# (IV) TO ASCII INTEGER (JV)
C     ****************************************************************
C
      IF(IV.LT.0.OR.IV.GT.9999999) GO TO 20
C
      WRITE(JV,10)IV
   10 FORMAT(I8,'    ')
      RETURN
C
   20 X=IV
      WRITE(JV,25)X
   25 FORMAT(5PE11.4,' ')
      CALL ISBYTE(X20,JV,6)
      CALL ISBYTE(X20,JV,7)
      CALL ISBYTE(X20,JV,9)
      CALL SQUEZL(JV,2,11)
      RETURN
      END
