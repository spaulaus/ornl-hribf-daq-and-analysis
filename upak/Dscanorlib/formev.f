C$PROG FORMEV    - Formats event-data for routine DDAT
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE FORMEV(IBUF,OBUF,IA,IB,NUM)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*2    IBUF(*)
C
      CHARACTER*6  OBUF(*)
C
      INTEGER*4    IA,IB,NUM,IT,N,I
C
      INTEGER*2    XFFFF
      DATA         XFFFF/'FFFF'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      N=0
C
      DO 100 I=IA,IB
C
      N=N+1
C
      IF(IBUF(I).EQ.XFFFF) THEN
      OBUF(N)='  FFFF'
      GO TO 100
      ENDIF
C
      IF(IBUF(I).GE.0)      THEN
      WRITE(OBUF(N),10)IBUF(I)
   10 FORMAT(I6)
      GO TO 100
      ENDIF
C
      IT=IAND(IBUF(I),'0FFF'X)
      WRITE(OBUF(N),20)IT
   20 FORMAT(' P',I4.4)
C
  100 CONTINUE
C
      NUM=N
C
      RETURN
      END
