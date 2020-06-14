C$PROG SETFA     - Set WIDTH, ASLO, ASHI for peaks in specified range
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SETFA(IERR)
C   
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C     ------------------------------------------------------------------
      CHARACTER*4  KMD
C   
      INTEGER*4 LIST(78)
C   
      EQUIVALENCE (KMD,LWD(1,1)),(LIST,LWD(1,2))
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
C   
C     ------------------------------------------------------------------
C     SET WIDTH, ASLO, ASHI - FOR PEAKS WITHIN SPECIFIED RANGE
C     ------------------------------------------------------------------
C   
      SETLO=0.0
      SETHI=16383.0
      IF(NF.LT.2) GO TO 10
C   
      CALL MILV(LIST(1),IV,SETLO,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      IF(NF.LT.3)   GO TO 10
      CALL MILV(LIST(3),IV,SETHI,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
C   
   10 TWA=FWA
      TWB=FWB
      TWC=FWC
      ASLOT=ASLO
      ASHIT=ASHI
C   
      IF(KMD.EQ.'SET-') GO TO 50
      IF(KMD.EQ.'SETW') GO TO 20
      IF(KMD.EQ.'SETL') GO TO 30
      IF(KMD.EQ.'SETH') GO TO 40
      GO TO 100
C   
   20 IF(NF.LT.4) GO TO 50
      TWA=0.0
      TWB=0.0
      TWC=0.0
      CALL MILV(LIST(5),IV,TWA,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      IF(NF.LT.5) GO TO 50
      CALL MILV(LIST(7),IV,TWB,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      IF(NF.LT.6) GO TO 50
      CALL MILV(LIST(9),IV,TWC,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      GO TO 50
C   
   30 IF(NF.LT.4) GO TO 50
      CALL MILV(LIST(5),IV,ASLOT,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      GO TO 50
C   
   40 IF(NF.LT.4) GO TO 50
      CALL MILV(LIST(5),IV,ASHIT,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
C   
   50 DO 60 I=1,NPAT
      X=PAT(I,1)
      IF(X.LT.SETLO.OR.X.GT.SETHI) GO TO 60
      IF(KMD.EQ.'SET-'.OR.KMD.EQ.'SETW')PAT(I,2)=TWA+TWB*SQRT(X)+TWC*X
      IF(KMD.EQ.'SET-'.OR.KMD.EQ.'SETL')PAT(I,3)=ASLOT
      IF(KMD.EQ.'SET-'.OR.KMD.EQ.'SETH')PAT(I,4)=ASHIT
   60 CONTINUE
      RETURN
C   
  100 IERR=1
      RETURN
      END
