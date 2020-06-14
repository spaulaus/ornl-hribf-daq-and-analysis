C$PROG NXBL      - Returns location of next blank in string
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION NXBL(IBY,IA,IB)
C
      BYTE IBY(*),BLANK
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO RETURN LOCATION OF NEXT BLANK
C     ------------------------------------------------------------------
C
      BLANK=Z'20'
C
      DO 10 I=IA,IB
      IF(IBY(I).EQ.BLANK) GO TO 20
   10 CONTINUE
      I=0
   20 NXBL=I
      RETURN
      END
