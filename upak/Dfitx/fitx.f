C$PROG FITX      - Linerar & Non-linear Least-Squares Fitting Program
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/FT02/ LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
      INTEGER*4 IWDRED(20)
C
      CHARACTER*4  KMD,IDONE,IWD1
C
      EQUIVALENCE (KMD,LWD(1,1)),(IWD1,IWD(1))
C
      DATA LCI,LIN,LCM/5,5,3/
C     ------------------------------------------------------------------
C
      CALL FITXNIT
C
      GO TO 100
C   
   50 IF(LIN.NE.LCI) THEN
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
                     ENDIF
      LIN=LCI
C   
  100 IF(LIN.EQ.LCI) WRITE(6,105)
  105 FORMAT(' fitx->',$)
      READ(LIN,110,END=50,ERR=50)IWD
  110 FORMAT(20A4)
      MSGF='    '
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
      IWDRED(I)=IWD(I)
  115 CONTINUE
      CALL CASEUP1(IWDRAW)
      CALL CASEUP(IWD)
C
      IF(IWD1.EQ.'LON ') GO TO 120
      IF(IWD1.EQ.'LOF ') GO TO 120
C
      IF(IWD1.EQ.'    ') GO TO 130
      IF(IWD1.EQ.'COM ') GO TO 130
      GO TO 140
C
  120 LISFLG=IWD1
      GO TO 100
C   
  130 WRITE(LOGUP,135)IWDRED
  135 FORMAT(1H ,20A4)
C
      IF(LIN.NE.LCI) THEN
                     WRITE(CMSSG,110)IWDRED
                     CALL MESSLOG(LOGUT,0)
                     GO TO 100
                     ENDIF
      GO TO 100
C   
  140 WRITE(CMSSG,110)IWD
      IF(LIN.EQ.LCI) CALL MESSLOG(0,LOGUP)
      IF(LIN.NE.LCI) CALL MESSLOG(LOGUT,LOGUP)
C   
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C   
      IF(KMD.EQ.'END ') STOP
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPER(LIN,LCI)
                        GO TO 100
                        ENDIF
      IDONE='    '
      IERR=0
      CALL CALLER(IDONE,IERR)
      IF(IERR.NE.0)       GO TO 50
      IF(IDONE.EQ.'YES ') GO TO 100
C   
      WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
C$PROG CALLER
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      IF(IDONE.NE.'YES ') CALL EQUATE(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL MODFIN(IERR)
      IF(IERR.NE.0)       RETURN
C   
      IF(IDONE.NE.'YES ') CALL LWDMOD
C
      IF(IDONE.NE.'YES ') CALL CMPGEN(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL CMPFIT(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPFITU(IDONE,IERR)
C
C   
      RETURN
      END
