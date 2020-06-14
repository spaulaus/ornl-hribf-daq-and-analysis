C$PROG SQUEZL    - Remove blanks from specified string & sqeeze left
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE SQUEZL(IBY,IA,IB)
C
      INTEGER*1  IBY(*),JBY(80)   !Was type BYTE on Alpha
C
      INTEGER*1  BLANK
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO REMOVE BLANKS FROM IWD (IA THRU IB) AND SQUEEZ LEFT
C     ------------------------------------------------------------------
C
      BLANK=Z'20'
      J=0
      N=0
      DO 10 I=IA,IB
      JTEMP=IBY(I)
      J=J+1
      JBY(J)=BLANK
      IF(JTEMP.EQ.BLANK) GO TO 10
      N=N+1
      JBY(N)=JTEMP
   10 CONTINUE
      N=IA
      DO 20 I=1,J
      IBY(N)=JBY(I)
      N=N+1
   20 CONTINUE
      RETURN
      END
