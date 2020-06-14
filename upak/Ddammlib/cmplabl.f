C$PROG CMPLABL   - Command processor for labeling operations
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE CMPLABL(IDONE,IERR)
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
      CHARACTER*4  IDONE,KMD
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      DATA IDW/1/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'WIN ') GO TO 100
C
      IF(KMD.EQ.'XYF ') GO TO 200
      IF(KMD.EQ.'XYC ') GO TO 200
      IF(KMD.EQ.'XYD ') GO TO 200
      IF(KMD.EQ.'XYP ') GO TO 200
      IF(KMD.EQ.'XYI ') GO TO 200
C
      IF(KMD.EQ.'LA7 ') GO TO 300
      IF(KMD.EQ.'LA8 ') GO TO 300
      IF(KMD.EQ.'LA9 ') GO TO 300
      IF(KMD.EQ.'LAZ7') GO TO 300
      IF(KMD.EQ.'LAZ8') GO TO 300
      IF(KMD.EQ.'LAZ9') GO TO 300
      IF(KMD.EQ.'LAL ') GO TO 300
      IF(KMD.EQ.'LAC ') GO TO 300   
C
      RETURN
C
  100 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      IF(IV.GT.20)  GO TO 1000
      IDW=IV
      GO TO 2500
C
  200 CALL XYMAN(IDW,IERR)
      GO TO 2500
C
  300 CALL LABLOD
      GO TO 2500
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('SYNTAX ERROR OR ILLEGAL WINDOW SPECIFICATION')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
C
 2500 IDONE='YES '
      RETURN
      END
