C$PROG PEAKADD   - Adds peaks to library (via command line input)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE PEAKADD(IERR)
C   
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM17/ IORD(500),ITEM(500),LOUXX(14,50)
C   
      INTEGER*4 LAT(500,15),LIST(78)
C   
      EQUIVALENCE (LAT,PAT),(LIST,LWD(1,2))
C   
      SAVE
C
C     ------------------------------------------------------------------
C     ADD TO PEAK-ATTRIBUTE-TABLE 
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(NPAT.GE.MAXPAT) GO TO 100
      NPAT=NPAT+1
C   
      I=1
      DO 10 J=1,4
      CALL MILV(LIST(I),IV,PAT(NPAT,J),KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      I=I+2
   10 CONTINUE
C   
      IF(PAT(NPAT,2).GT.0.1) GO TO 20
      PAT(NPAT,2)=FWA+FWB*SQRT(PAT(NPAT,1))+FWC*PAT(NPAT,1)
C   
   20 DO 30 K=6,9
      CALL MILV(LIST(I),LAT(NPAT,K),XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      I=I+2
   30 CONTINUE
      NUPAT=1
C   
      LAT(NPAT,10)=1
      LAT(NPAT,11)=0
      LAT(NPAT,12)=0
C   
C     ------------------------------------------------------------------
C     RE-ORDER "PAT"
C     ------------------------------------------------------------------
C   
      IF(NPAT.LE.1) RETURN
      CALL FINORD(PAT(1,1),IORD,ITEM,NPAT)
      DO 40 J=1,15
      CALL IORDER(LAT(1,J),IORD,ITEM,NPAT)
   40 CONTINUE
C   
      RETURN
C   
  100 NPAT=NPAT-1
      IERR=1
      RETURN
      END
