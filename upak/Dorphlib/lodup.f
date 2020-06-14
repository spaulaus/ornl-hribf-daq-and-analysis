C$PROG LODUP     - Loads bytes IA-IB from IBY to JBY starting at JA 
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE LODUP(IBY,IA,IB,JBY,JA)
C
      BYTE IBY(*),JBY(*)
C
      SAVE
C
C     ------------------------------------------------------------------
C     LOADS BYTES IA THRU IB FROM IBY INTO JBY (STARTING AT JA IN JBY)
C     ------------------------------------------------------------------
C
      J=JA
      DO 10 I=IA,IB
      JBY(J)=IBY(I)
      J=J+1
   10 CONTINUE
      RETURN
      END
