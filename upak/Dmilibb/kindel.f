C$PROG KINDEL    - Classifies a delimiter as to kind: ( + - * /
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      FUNCTION KINDEL(IWD)
C
      DIMENSION IDEL(5)
C
      DATA (IDEL(I),I=1,4)/Z'2B',Z'2D',Z'2A',Z'2F'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO CLASSIFY DELIMITER AS TO KIND ( + - * /)
C     ------------------------------------------------------------------
C   
      DO 10 I=1,4
      IF(IWD.EQ.IDEL(I)) GO TO 20
   10 CONTINUE
      I=0
   20 KINDEL=I
      RETURN
      END
