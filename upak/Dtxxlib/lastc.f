C$PROG LASTC
      FUNCTION LASTC(NC,NHI)
      INTEGER*4 NC(1)
C
      INTEGER*4  X20
      DATA       X20/'20'X/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
C     FUNCTION TO RETURN LOCATION OF LAST NON-BLANK
C   
      DO 20 J=1,NHI
      K=NHI-J+1
      IF(NC(K).NE.X20) GO TO 30
   20 CONTINUE
      K=0
   30 LASTC=K
      RETURN
      END
