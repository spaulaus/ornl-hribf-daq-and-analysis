C$PROG NUMFORB   - Number formatting routine
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE NUMFORB(KV,NUMA)
C
      CHARACTER*8  NUMA
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IV=IABS(KV)
C
      IF(IV.LT.100000000) GO TO 100
C
      JV=IV/10000
      IEX=4
      IF(KV.LT.0) JV=-JV
      WRITE(NUMA,60)JV,IEX
   60 FORMAT(I6,1H+,I1)
      RETURN
C
  100 WRITE(NUMA,110)KV
  110 FORMAT(I8)
      RETURN
      END
