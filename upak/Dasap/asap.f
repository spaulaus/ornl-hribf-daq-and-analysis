C$PROG ASAP
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
      COMMON/HEPF/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/III/ LIN,LCM,LCI
C     ------------------------------------------------------------------
      CHARACTER*4  KMD,IWD1,IDONE
C
      EQUIVALENCE (KMD,LWD(1,1)),(IWD1,IWD(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C     AUTOMATIC SPECTRUM ANALYSIS PROGRAM - ULTRIX VERSION
C     ------------------------------------------------------------------
C
      CALL SAPNIT
C
      IF(IHEPF.NE.'YES ') GO TO 100
C
      WRITE(LOGUT,30)
      WRITE(LOGUT,35)
      WRITE(LOGUT,40)
   30 FORMAT(1H ,
     &'Type: h       - for list of HELP code-words & subjects')
   35 FORMAT(1H ,
     &'Type: h all   - for a more detailed help directory')
   40 FORMAT(1H ,
     &'Type: h code  - for command list for associated subject')
C
      GO TO 100
C   
   50 LIN=LCI
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
C   
  100 IF(LIN.EQ.LCI) WRITE(6,105)
  105 FORMAT(' ASAP->',$)
      READ(LIN,110,END=50,ERR=50)IWD
  110 FORMAT(20A4)
      MSGF='    '
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
  115 CONTINUE
      CALL CASEUP1(IWDRAW)
      CALL CASEUP(IWD)
C
      IF(IWD1.EQ.'    ') GO TO 130
      IF(IWD1.EQ.'COM ') GO TO 130
      GO TO 140
C   
  130 IF(LIN.NE.LCI) THEN
                     WRITE(CMSSG,110)IWD
                     CALL MESSLOG(LOGUT,LOGUP)
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
      CALL CALLER(IDONE,IERR)
      IF(IDONE.EQ.'YES ') GO TO 100
C   
      WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
