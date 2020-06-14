C$PROG SWAPF     - Swaps bytes in full-word (32-bit) array
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/13/99
C     ******************************************************************
C
      SUBROUTINE SWAPF(IBUF,IA,IB) 
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4 IBUF(1),ITMP,JTMP,IA,IB,I
C
      BYTE IBY(4),JBY(4)
C
      EQUIVALENCE (IBY,ITMP),(JBY,JTMP)
C     ------------------------------------------------------------------
C
      DO 10 I=IA,IB
      ITMP=IBUF(I)
      JBY(1)=IBY(4)
      JBY(2)=IBY(3)
      JBY(3)=IBY(2)
      JBY(4)=IBY(1)
      IBUF(I)=JTMP
   10 CONTINUE
      RETURN
      END
