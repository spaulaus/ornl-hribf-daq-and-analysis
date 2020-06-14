C$PROG IFIND     - Returns location of "test byte"
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      FUNCTION IFIND(IBY,ITST,IA,IB)
C
      BYTE IBY(*)
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO RETURN THE LOCATION OF TEST BYTE "ITST" IN IBY
C     ------------------------------------------------------------------
C
      DO 10 I=IA,IB
      JTEMP=IBY(I)
      IF(JTEMP.EQ.ITST) GO TO 20
   10 CONTINUE
      I=0
   20 IFIND=I
      RETURN
      END
