C$PROG ISWAF     - Does full-word byte-swap of 1 word
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/17/99
C     ******************************************************************
C
      INTEGER*4 FUNCTION ISWAF(IWD)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4 IWD,ITMP,JTMP
C
      BYTE IBY(4),JBY(4)
C
      EQUIVALENCE (IBY,ITMP),(JBY,JTMP)
C
C     ------------------------------------------------------------------
C     SWAPS BYTES FOR ONE FULL-WORD "IWD"
C     ------------------------------------------------------------------
C
      ITMP=IWD
      JBY(1)=IBY(4)
      JBY(2)=IBY(3)
      JBY(3)=IBY(2)
      JBY(4)=IBY(1)
      ISWAF=JTMP
      RETURN
      END
