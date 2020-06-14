C$PROG LDFSWAP   - Manages byte-swapping for routine LDFREAD
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE LDFSWAP(KIND,IDAT)
C   
C     ------------------------------------------------------------------
      INTEGER*4 ITM,JTM
C
      BYTE      IBY(4),JBY(4)
C
      EQUIVALENCE (IBY,ITM),(JBY,JTM)
C
      SAVE
C
C     ------------------------------------------------------------------
C     SWAPS BYTES IN IDAT - KIND=0 SAYS DO NOTHING
C                         - KIND=1 SWAP BYTES IN EACH HALF-WORD
C                         - KIND=2 SWAP BYTES IN      FULL-WORD
C     ------------------------------------------------------------------
C   
      IF(KIND.EQ.0)       RETURN
C   
      ITM=IDAT
C   
      IF(KIND.EQ.2) GO TO 10
C   
      JBY(1)=IBY(2)
      JBY(2)=IBY(1)
      JBY(3)=IBY(4)
      JBY(4)=IBY(3)
      IDAT=JTM
      RETURN
C   
   10 JBY(1)=IBY(4)
      JBY(2)=IBY(3)
      JBY(3)=IBY(2)
      JBY(4)=IBY(1)
      IDAT=JTM
      RETURN
      END
