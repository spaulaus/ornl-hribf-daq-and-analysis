C$PROG DAMM      - Damm - Display, Analysis & Manipulation Module
      program damm
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 08/20/2005
C     ******************************************************************
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
      character*4 cnamprog(2)
      equivalence (cnamprog, namprog)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
      character*4 cnamfil(20,20)
      equivalence (cnamfil, namfil)
C     ------------------------------------------------------------------
      COMMON/HEPF/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/DML6/ ISIGNF
      CHARACTER*4  ISIGNF
C     ------------------------------------------------------------------
      INTEGER*4    NAMHEP(6)
      CHARACTER*24 CNAMHEP
      EQUIVALENCE  (CNAMHEP,NAMHEP)
      DATA CNAMHEP/'damm.hep'/
C
      INTEGER*4    X2A
      DATA         X2A/Z'2A'/
C
      CHARACTER*4  KMD,IDONE,IWD1
C   
      EQUIVALENCE (KMD,LWD(1,1)),(IWD1,IWD(1))
C
      DATA cNAMFIL,KFIL/420*'    '/
C
      DATA LCM,LCI,LIN/4,5,5/
C
      DATA ISIGNF/'USDA'/
C
      DATA cNAMPROG/'DAMM','    '/
C   
C     *************************************************************
C
      CMSSG=' '
      LOGUT=6
      LOGUP=7
      LISFLG='LOF '
      MSGF='    '
C
      CALL DAMMMSG
C
      CALL HELPOPEN(1,NAMHEP,IHEPF)            !OPEN HELP-FILE
C
      OPEN(UNIT      = 7,                      !OPEN/CREATE LOG-FILE
     &     FILE      = 'damm.log',
     &     STATUS    = 'UNKNOWN',
     &     ACCESS    = 'APPEND',
     &     IOSTAT    = IOS)
C
      IF(IOS.NE.0) THEN 
                   CALL IOFERR(IOS)
                   LOGUP=0
                   WRITE(LOGUT,25)
                   ENDIF        
C
   25 FORMAT('OUTPUT TO LOG-FILE IS DISABLED')
C
      CALL CTCNIT
C
      IF(IHEPF.NE.'YES ') GO TO 100
C
      WRITE(LOGUT,30)
      WRITE(LOGUT,35)
      WRITE(LOGUT,40)
   30 FORMAT(
     &'Type: h       - for list of HELP code-words & subjects')
   35 FORMAT(
     &'Type: h all   - for a more detailed help directory')
   40 FORMAT(
     &'Type: h code  - for command list for associated subject')
C 
      GO TO 100
C   
C     *************************************************************
C   
   50 LIN=LCI
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
      MSGF='    '
C   
  100 IF(MSGF.NE.'    ') GO TO 50
C
      IF(LIN.EQ.LCI) WRITE(LOGUT,105)
  105 FORMAT('DAMM->',$)
C
      READ(LIN,110,END=50,ERR=50)IWD
  110 FORMAT(20A4)
C   
      MSGF='    '
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
  115 CONTINUE
      CALL CASEUP1(IWDRAW)
C
      CALL CASEUP(IWD)
C   
      CALL ILBYTE(ITW,IWD,0)
      IF(IWD1.EQ.'    ') GO TO 130
      IF(IWD1.EQ.'COM ') GO TO 130
      IF(ITW.EQ.X2A)     GO TO 130
      GO TO 140
C   
  130 WRITE(CMSSG,110)IWDRAW
      IF(LIN.EQ.LCI) CALL MESSLOG(0,LOGUP)
      IF(LIN.NE.LCI) CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
C   
  140 WRITE(CMSSG,110)IWDRAW
      IF(LIN.EQ.LCI) CALL MESSLOG(0,LOGUP)
      IF(LIN.NE.LCI) CALL MESSLOG(LOGUT,LOGUP)
C   
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C   
      IF(KMD.EQ.'END ') STOP
C   
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPER(LIN,LCI)
                        GO TO 100
                        ENDIF
C   
      IDONE='    '
      IERR=0
C
      CALL CALLER(IDONE,IERR)
C   
      IF(IDONE.EQ.'YES ') GO TO 100
C   
      WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
C$PROG CALLER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/20/2005
C     ************************************************************
C
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      IF(IDONE.NE.'YES ') CALL EQUATE(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL MODFIN(IERR)
      IF(IERR.NE.0)       RETURN
C
      IF(IDONE.NE.'YES ') CALL MANOPEN(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL LWDMOD
C
      IF(IDONE.NE.'YES ') CALL CMPGEN(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL CMPLABL(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPSAM(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPPLO(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPMILDO(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPTDX(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPXAM(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPHGEN(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPFADD(IDONE,IERR)
C
      RETURN
      END
