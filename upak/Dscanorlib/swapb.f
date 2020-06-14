C$PROG SWAPB     - Swaps bytes in half-word array LIST
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/01/98
C     ******************************************************************
C
      SUBROUTINE SWAPB(LIST,IA,IB)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*2 LIST(1),IA,IB,ITMP,JTMP,I
C
      BYTE IBY(2),JBY(2)
C
      EQUIVALENCE (IBY,ITMP),(JBY,JTMP)
C     ------------------------------------------------------------------
C
      DO 10 I=IA,IB
      ITMP=LIST(I)
      JBY(1)=IBY(2)
      JBY(2)=IBY(1)
      LIST(I)=JTMP
   10 CONTINUE
      RETURN
      END
