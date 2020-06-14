C$PROG SQUEZLX   - Squeeze left - characters in range of IA to IB
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SQUEZLX(IBY,IA,IB)
C
      BYTE IBY(*),X20,X00
C
      DATA X20,X00/Z'20',Z'00'/
C
      CHARACTER*4  LBL
C
      SAVE
C
C     ------------------------------------------------------------------
C
      LBL='YES '
      II=IA-1
      DO 20 I=IA,IB
      IF(IBY(I).EQ.X20.AND.LBL.EQ.'YES ') GO TO 20
      IF(IBY(I).EQ.X00) GO TO 20
      II=II+1
      IBY(II)=IBY(I)
      LBL='NO  '
      IF(IBY(I).EQ.X20) LBL='YES '
   20 CONTINUE
      JA=II+1
      DO 30 J=JA,IB
      IBY(J)=X20
   30 CONTINUE
      RETURN
      END
