C$PROG SWAPAT    - Loads peak-attributes to/from library
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SWAPAT(MODE)
C   
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C   
      COMMON/SM03/ XP(4,44),XPSAV(4,44),IPO(176),JPO(176)
C   
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM16/ IHOLF(4,44),JPU(44),XOR(44)
C
      SAVE
C
C     ==================================================================
C   
      IF(MODE.EQ.1) GO TO 100
      IF(MODE.EQ.2) GO TO 200
      RETURN
C   
  100 DO 110 N=1,NPK
      I=JPU(N)
      TMP=PAT(I,1)
      PAT(I,1)=XP(1,N)
      XP(1,N)=TMP
  110 CONTINUE
      RETURN
C   
  200 DO 210 N=1,NPK
      I=JPU(N)
      TMP=PAT(I,1)
      PAT(I,1)=XP(1,N)
      XP(1,N)=TMP
  210 CONTINUE
      RETURN
      END
