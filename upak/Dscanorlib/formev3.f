C$PROG FORMEV3   - Formats L003 event-data for routine DDAT
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/19/99
C     ******************************************************************
C
      SUBROUTINE FORMEV3(IBUF,OBUF,IA,IB,NUM)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*2    IBUF(*)
C
      CHARACTER*6  OBUF(*)
C
      INTEGER*4    IA,IB,JB,NUM,IT,N,I
C
      INTEGER*4    XFFFF,X7FFF
C
      DATA         XFFFF,X7FFF/'FFFF'X,'7FFF'X/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      N=0
C
      JB=IB-2
C
      DO 100 I=IA,JB,2
      IT=IAND(IBUF(I),X7FFF)
      N=N+1
      WRITE(OBUF(N),20)IT
   20 FORMAT('P',I5.5)
C
      IT=IAND(IBUF(I+1),XFFFF)
      N=N+1
      WRITE(OBUF(N),30)IT
   30 FORMAT(I6)
C
  100 CONTINUE
C
      N=N+1
      OBUF(N)='  FFFF'
      N=N+1
      OBUF(N)='  FFFF'
C
      NUM=N
C
      RETURN
      END
