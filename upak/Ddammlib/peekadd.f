C$PROG PEEKADD   - Adds peaks to library (via cursor input)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE PEEKADD(X)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM17/ IORD(500),ITEM(500),LOUXX(14,50)
C   
      INTEGER*4 LAT(500,15)
C   
      EQUIVALENCE (LAT,PAT)
C
      SAVE
C
C     ------------------------------------------------------------------
C     ADD TO PEAK-ATTRIBUTE-TABLE 
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(X.LE.0.0)       RETURN
      IF(NPAT.GE.MAXPAT) GO TO 100
C   
      DO 10 N=1,NPAT
      DMIN=0.5*(FWA+FWB*SQRT(X)+FWC*X)
      IF(DMIN.LT.2.0) DMIN=2.0
      DIF=ABS(X-PAT(N,1))
      IF(DIF.LE.DMIN) RETURN
   10 CONTINUE
C   
      NPAT=NPAT+1
C   
      PAT(NPAT,1)=X
      PAT(NPAT,2)=FWA+FWB*SQRT(PAT(NPAT,1))+FWC*PAT(NPAT,1)
      PAT(NPAT,3)=ASLO
      PAT(NPAT,4)=ASHI
      PAT(NPAT,11)=X
C   
      DO 30 K=5,9
      LAT(NPAT,K)=0
   30 CONTINUE
      LAT(NPAT,10)=1
      LAT(NPAT,12)=0
      NUPAT=1
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
      WRITE(CMSSG,105)
  105 FORMAT('PEAK LIBRARY OVERFLOW - DELETE SOME PEAKS, ETC')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
