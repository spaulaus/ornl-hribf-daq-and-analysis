C$PROG LSNB      - Returns location of last non-blank in string
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION LSNB(IBY,IA,IB)
C
      BYTE IBY(*),BLANK
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO RETURN LOCATION OF LAST NON-BLANK
C     ------------------------------------------------------------------
C
      BLANK=Z'20'
C
      I=IB+1
   10 I=I-1
      IF(I.LT.IA)         GO TO 20
      IF(IBY(I).EQ.BLANK) GO TO 10
      LSNB=I
      RETURN
C
   20 LSNB=0
      RETURN
      END
