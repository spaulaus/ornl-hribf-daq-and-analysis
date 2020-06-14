C$PROG SAVUM     - Save some or all fit results in PAT
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SAVUM(IERR)
C   
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C   
      COMMON/SM03/ XP(4,44),XPSAV(4,44),IPO(176),JPO(176)
C   
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM16/ IHOLF(4,44),JPU(44),XOR(44)
C   
      COMMON/SM10/ ERR(18),ISAV,KFIT
C     ------------------------------------------------------------------
      INTEGER*4 LIST(78)
C
      CHARACTER*4  KMD
C   
      EQUIVALENCE (KMD,LWD(1,1)),(LIST,LWD(1,2))
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(KMD.EQ.'SAV ') GO TO 10
      IF(KMD.EQ.'SAX ') GO TO 20
      IF(KMD.EQ.'SAW ') GO TO 30
      IF(KMD.EQ.'SAL ') GO TO 40
      IF(KMD.EQ.'SAH ') GO TO 50
C   
      GO TO 200
C   
C     ------------------------------------------------------------------
C     SAVE SOME OR ALL OF "FIT RESULTS" IN "PAT"
C     ------------------------------------------------------------------
C   
   10 KSAV1=1
      KSAV2=4
      GO TO 60
C   
   20 KSAV1=1
      KSAV2=1
      GO TO 60
C   
   30 KSAV1=2
      KSAV2=2
      GO TO 60
C   
   40 KSAV1=3
      KSAV2=3
      GO TO 60
C   
   50 KSAV1=4
      KSAV2=4
C   
   60 ISAV=1                    !INHIBIT GFIT FROM RE-ESTIMATING WIDTH
      IF(NUPAT.NE.0) GO TO 200
      IF(NF.EQ.1) GO TO 90
      IF(NF.EQ.2) GO TO 80
      IF(NF.EQ.3) GO TO 70
      GO TO 200
C   
   70 CALL MILV(LIST(1),JSAV1,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
      CALL MILV(LIST(3),JSAV2,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
      GO TO 100
C   
   80 CALL MILV(LIST(1),JSAV1,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
      JSAV2=JSAV1
      GO TO 100
C   
   90 JSAV1=1
      JSAV2=NPK
C   
  100 IF(JSAV2.GT.NPK) JSAV2=NPK
      IF(JSAV1.LT.1.OR.JSAV1.GT.JSAV2) GO TO 200
C   
      DO 120 K=KSAV1,KSAV2
      DO 110 J=JSAV1,JSAV2
      I=JPU(J)
      PAT(I,K)=XP(K,J)
      IF(K.EQ.2) PAT(I,K)=1.66478*PAT(I,K)
  110 CONTINUE
  120 CONTINUE
      RETURN
C   
  200 IERR=1
      RETURN
      END
