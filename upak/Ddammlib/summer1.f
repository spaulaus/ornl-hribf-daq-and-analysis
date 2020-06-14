C$PROG SUMMER1   - Processes sum-requests from CMPPLO
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SUMMER1
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
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     ------------------------------------------------------------------
      INTEGER*4    IDL(10),NCHL(10),ANUM(3),NID
C
      CHARACTER*4  CLWD(2,40),KSOT(7),KFL(10),TEST,READ
C
      REAL*8       SUMI
C
      CHARACTER*4  KMD,KF
C
      EQUIVALENCE (KMD,LWD(1,1)),(CLWD,LWD)
C
      DATA TEST,READ/'TEST','READ'/
C
      DATA NID,ILOC,IHIC/0,0,8191/
C
      DATA KSOT/'M   ','N   ','O   ','P   ','Q   ','R   ','S   '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO PROCESS SUM REQUESTS
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'SUM ') GO TO 200
C
      IF(NF.NE.3) THEN
                  ILOC=0
                  IHIC=8191
                  RETURN
                  ENDIF
C
      CALL LIMIV(LWD(1,2),   0,16383,ILOC,IERR)
      IF(IERR.NE.0) GO TO 510
      CALL LIMIV(LWD(1,3),ILOC,16383,IHIC,IERR)
      IF(IERR.NE.0) GO TO 510
      RETURN
C
  200 IF(NF.LT.2) GO TO 220
      KF='N   '
      NID=0
C
      DO 210 I=2,NF
C
      DO 205 JJ=1,7
      IF(CLWD(1,I).NE.KSOT(JJ)) GO TO 205
      KF=KSOT(JJ)
      GO TO 210
  205 CONTINUE
C
      NID=NID+1
      IF(NID.GT.10) GO TO 550
      CALL IVALU(LWD(1,I),IDL(NID),IERR)
      IF(IERR.NE.0) GO TO 510
      KFL(NID)=KF
C
  210 CONTINUE
C
  220 DO 230 N=1,NID
      CALL SPKIN1(TEST,KFL(N),IDL(N),1.0,0,0,NCHL(N),IERR)
      IF(IERR.NE.0) RETURN
  230 CONTINUE
C
      DO 250 N=1,NID
      ILO=ILOC+1
      IF(ILO.GT.NCHL(N)) GO TO 250
      IHI=IHIC+1
      IF(IHI.GT.NCHL(N)) IHI=NCHL(N)
      IF(ILO.GT.IHI)     GO TO 250
C
      CALL SPKIN1(READ,KFL(N),IDL(N),1.0,ILO,IHI,NCHL(N),IERR)
C
      JLOC=ILO-1
      JHIC=IHI-1
      NCH=IHI-ILO+1
      ZUMXXY=0.0
      ZUMXY=0.0
      ZUMY=0.0
      XNOW=0.0
      SUMI=0.0D0
C
      DO 240 I=1,NCH
      IDAT=IDATF(I)
      SUMI=SUMI+IDAT
      CRAW=FLOAT(IDAT)
      ZUMXXY=ZUMXXY+XNOW*XNOW*CRAW
      ZUMXY=ZUMXY+XNOW*CRAW
      ZUMY=ZUMY+CRAW
      XNOW=XNOW+1.0
  240 CONTINUE
C
      IF(ZUMY.LT.1.0) ZUMY=1.0
      CNR=ZUMXY/ZUMY
      SIGSQ=ZUMXXY/ZUMY-CNR*CNR
      IF(SIGSQ.LT.0.0001) SIGSQ=0.0001
      FWR=2.354*SQRT(SIGSQ)
      CNR=CNR+FLOAT(JLOC)
C
      CALL DFASCII(SUMI,ANUM,11)
C
      WRITE(CMSSG,245)KFL(N),IDL(N),JLOC,JHIC,ANUM,CNR,FWR
  245 FORMAT('FOR ',A2,I8,' (',I5,' -',I5,')  SUM,CENT,FWHM=',
     &2A4,A3,2F9.2)
      CALL MESSLOG(LOGUT,LOGUP)
C
  250 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     RETURN ERROR MESSAGES
C     ------------------------------------------------------------------
C
  510 WRITE(CMSSG,515)
  515 FORMAT('SYNTAX ERROR OR ILLEGAL COMMAND - IGNORED')
      GO TO 600
C
  550 WRITE(CMSSG,555)
  555 FORMAT('MAX NUMBER OF ID,S ALLOWED IS 10 - TRY AGAIN')
C
  600 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
