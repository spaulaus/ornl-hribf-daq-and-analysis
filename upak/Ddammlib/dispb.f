C$PROG DISPB     - Displays peaks between cmin & cmax
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/12/02
C     ******************************************************************
C
      SUBROUTINE DISPB(IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM09/ XYP(50,2),NBXY
C   
      COMMON/SM20/ RESUL(12,44),FOUT(50),ITRITL(10)
C
C     ------------------------------------------------------------------
C   
      INTEGER*4    LAT(500,15),LIST(78)
C
      CHARACTER*4  KMD
C   
      EQUIVALENCE (LAT,PAT),(LIST,LWD(1,2)),(KMD,LWD(1,1))
C
      SAVE
C   
C     ------------------------------------------------------------------
C     DISPLAY PEAKS BETWEEN CMIN AND CMAX OR BACKGROUND POINTS
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(KMD.EQ.'PRP ') GO TO 10
      IF(KMD.EQ.'PRB ') GO TO 60
      GO TO 100
C   
   10 CALL MILV(LIST(1),IV,CMIN,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      CALL MILV(LIST(3),IV,CMAX,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      IF(NF.EQ.3) GO TO 20
      CMIN=0.0
      CMAX=16383.0
   20 WRITE(CMSSG,30)
   30 FORMAT('  #       X   FWHM  ASLO  ASHI    RELINT  XWLHOF')
      CALL MESSLOG(LOGUT,LOGUP)
      DO 50 I=1,NPAT
      IF(PAT(I,1).LT.CMIN.OR.PAT(I,1).GT.CMAX) GO TO 50
      WRITE(CMSSG,40)I,(PAT(I,J),J=1,4), PAT(I,13),
     &                 (LAT(I,J),J=6,10),LAT(I,5)
   40 FORMAT(I3,F8.1,F7.2,2F6.3,F10.3,2X,6I1)
      CALL MESSLOG(LOGUT,LOGUP)
   50 CONTINUE
      RETURN
C   
   60 IF(NBXY.LE.0) RETURN
      N=0
      DO 70 I=1,NBXY
      N=N+1
      FOUT(N)=XYP(I,1)
      N=N+1
      FOUT(N)=XYP(I,2)
   70 CONTINUE
      CALL FAROUT(FOUT,0,N)
      RETURN
C   
  100 IERR=1
      RETURN
      END
