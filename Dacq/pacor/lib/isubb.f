C$PROG ISUBB
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE ISUBB(IBY,IA,IB,ICI,ICF)
C
      BYTE IBY(*)
C
C     ************************************************************
C     REPLACE ALL CHARACTERS "ICI" WITH "ICF" IN IBY WITHIN IA-IB
C     ************************************************************
C
      DO 10 I=IA,IB
      IF(IBY(I).EQ.ICI) IBY(I)=ICF
   10 CONTINUE
      RETURN
      END
