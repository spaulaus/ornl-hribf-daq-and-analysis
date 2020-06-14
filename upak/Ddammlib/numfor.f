C$PROG NUMFOR    - Number formatting routine
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE NUMFOR(KV,NUMA)
C
      CHARACTER*8  NUMA,STARS
C
      DATA         STARS/'******  '/
C
      SAVE
C     ------------------------------------------------------------------
C
      IV=IABS(KV)
C
      IF(IV.LT.1000000) GO TO 100
C
      JV=IV/1000
      IEX=3
      IF(JV.LT.10000) GO TO 50
C
      DO 20 I=4,9
      IEX=I
      JV=JV/10
      IF(JV.LT.10000) GO TO 50
   20 CONTINUE
      NUMA=STARS
      RETURN
C
   50 IF(KV.LT.0) JV=-JV
      WRITE(NUMA,60)JV,IEX
   60 FORMAT(I5,1H+,I1,' ')
      RETURN
C
  100 WRITE(NUMA,110)KV
  110 FORMAT(I7,' ')
      RETURN
      END
