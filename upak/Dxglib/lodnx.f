C$PROG LODNX     - Locates & loads next contiguous string
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE LODNX(IBY,IA,IB,JBY,JA)
C
      IMPLICIT NONE
C
      INTEGER*4  IA,IB,JA,IS,I,J
C
c     BYTE IBY(*),JBY(*),BLANK
      integer(kind=1) IBY(*),JBY(*),BLANK
C
      SAVE
C
C     ------------------------------------------------------------------
C     FINDS NEXT BLANK AND THEN
C     LOADS NEXT CONTIGUOUS STRING IN THE RANGE IA THRU IB
C     FROM IBY INTO JBY (STARTING AT JA IN JBY)
C     ------------------------------------------------------------------
C
      BLANK=Z'20'
C
      DO 10 I=IA,IB
      IF(IBY(I).EQ.BLANK) THEN
                          IS=I+1
                          GO TO 20
                          ENDIF
   10 CONTINUE
      RETURN
C
   20 J=JA
      DO 30 I=IS,IB
      IF(IBY(I).EQ.BLANK) THEN
                          IF(J.GT.JA) RETURN
                          GO TO 30
                          ENDIF
      JBY(J)=IBY(I)
      J=J+1
   30 CONTINUE
      RETURN
      END
