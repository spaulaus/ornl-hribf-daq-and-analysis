C$PROG FIELDER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE FIELDER(IWD,JA,JB,ILO,IHI,NF)
C
      INTEGER*4 IWD(*),ILO(*),IHI(*)
C
      SAVE
C
C     ************************************************************
C     FINDS CONTIGUOUS FIELDS IN IWD BETWEEN JA & JB
C     ************************************************************
C
      NF=0
      IB=JA
  100 IF(IB.GE.JB) GO TO 200
      IA=NXNB(IWD,IB,JB)
      IF(IA.LE.0) GO TO 200
      IB=NXBL(IWD,IA,JB)
      IF(IB.LE.0) IB=JB+1
      IB=IB-1
      NF=NF+1
      ILO(NF)=IA
      IHI(NF)=IB
      IB=IB+1
      GO TO 100
C
  200 RETURN
      END
