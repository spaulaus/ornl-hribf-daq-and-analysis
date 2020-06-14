C$PROG SWAPB     - Swaps bytes in half-words
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE SWAPB(BUF,IA,IB)
C   
      INTEGER*2 BUF(1),ITM,JTM
C
      BYTE IBY(2),JBY(2)
C
      EQUIVALENCE (IBY,ITM),(JBY,JTM)
C   
      DO 10 I=IA,IB
      ITM=BUF(I)
      JBY(1)=IBY(2)
      JBY(2)=IBY(1)
      BUF(I)=JTM
   10 CONTINUE
      RETURN
      END
