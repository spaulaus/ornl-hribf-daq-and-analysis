C$PROG SWAPUM    - Byte-swaps a buffer as specified by limit arrays
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE SWAPUM(JLO,JHI)
C   
C     ------------------------------------------------------------------
      COMMON/AAA/ ILO(8500),IHI(8500),NHW(8500),NHIS,LENT
C     ------------------------------------------------------------------
      COMMON/CCC/ IBUF(4096)
C     ------------------------------------------------------------------
C   
      INTEGER*2   IBUH(8192)
C   
      EQUIVALENCE (IBUH(1),IBUF(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      DO 100 N=1,NHIS
      IL=ILO(N)
      IH=IHI(N)
      IF(IL.GT.JHI) GO TO 100
      IF(IH.LT.JLO) GO TO 100
C   
      IF(IL.LT.JLO) IL=JLO
      IF(IH.GT.JHI) IH=JHI
C   
      JL=IL-JLO+1
      JH=IH-JLO+1
C
      CALL SWAPB(IBUH,JL,JH)
C   
      IF(NHW(N).NE.2) GO TO 100
C   
      CALL SWAPH(IBUH,JL,JH)
C   
  100 CONTINUE
      RETURN
      END
