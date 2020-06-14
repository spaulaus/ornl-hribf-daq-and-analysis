C$PROG STOPI     - Stopping Power program using Ziegler's stuff
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/19/2000
C     ******************************************************************
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
C
      INTEGER*4    IERR,I
C
      CHARACTER*4  IDONE,KMD,KMX,KMI
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(2,1)),(KMI,IWD(1))
C
      DATA LIN,LCI,LCM/5,5,9/
C
C     ******************************************************************
C
      CALL STOPINIT
C
      GO TO 100
C
   50 LIN=LCI
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 IF(LIN.EQ.LCI) WRITE(LOGUT,105)
  105 FORMAT(' STOPI->',$)
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
      IF(KMI.EQ.'COM ') GO TO 130
      GO TO 140
C
  130 IF(LIN.NE.LCI) THEN
                     WRITE(CMSSG,110)IWD
                     CALL MESSLOG(LOGUT,LOGUP)
                     GO TO 100
                     ENDIF
C
      WRITE(CMSSG,110)IWD
      CALL MESSLOG(0,LOGUP)
      GO TO 100
C
  140 WRITE(CMSSG,110)IWD
      IF(LIN.EQ.LCI) CALL MESSLOG(0,LOGUP)
      IF(LIN.NE.LCI) CALL MESSLOG(LOGUT,LOGUP)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMX.NE.'    ') GO TO 1000
C
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPER(LIN,LCI)
                        GO TO 100
                        ENDIF
C
      IDONE='    '
      CALL CALLER(IDONE,IERR)
      IF(IDONE.EQ.'YES ') GO TO 100
C
      WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      GO TO 1500
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Syntax Error - Command Ignored')
C
 1500 CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
C$PROG CALLER    - Routine caller
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/07/98
C     ******************************************************************
C
      SUBROUTINE CALLER(IDONE,IERR)
C
      INTEGER*4    IERR
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
      IF(IDONE.NE.'YES ') CALL CMPDEDX(IDONE,IERR)
C
C
      RETURN
      END
C$PROG STOPINIT  - Initializing routine for program stopo
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/18/2000
C     ******************************************************************
C
      SUBROUTINE STOPINIT
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/HEP/  IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      INTEGER*4    I
C     ------------------------------------------------------------------
      character*4  cnamprog(2)
      equivalence  (cnamprog, namprog)
      DATA        cNAMPROG/'STOP','I   '/
C     ------------------------------------------------------------------
C
      CMSSG=' '
      MSGF='    '
      LISFLG='LON '
      LOGUT=6
      LOGUP=7
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'stopi.log',
     &     STATUS     = 'UNKNOWN')
C
c     CLOSE(UNIT=LOGUP,DISP='DELETE')
      CLOSE(UNIT=LOGUP)
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'stopi.log',
     &     STATUS     = 'REPLACE')
C
      CALL HELPNIT(IHEPF)
C
      CALL CTCNIT
C
      RETURN
C
      END
C$PROG HELPNIT   - Opens stopi.hep
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/18/2000
C     ******************************************************************
C
      SUBROUTINE HELPNIT(IHEPF)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    NAMHEP(6),LHEP
C
      CHARACTER*4  IHEPF
C
      CHARACTER*24 CNAMHEP
C
      EQUIVALENCE (CNAMHEP,NAMHEP)
C
      DATA         LHEP/13/
C     ------------------------------------------------------------------
C
      CNAMHEP='stopi.hep'
C
      CALL HELPOPEN(LHEP,NAMHEP,IHEPF)
C
      IF(IHEPF.NE.'YES ') RETURN
C
      WRITE(6,30)
      WRITE(6,35)
      WRITE(6,40)
   30 FORMAT(1H ,
     &'Type: h       - for list of HELP code-words & subjects')
   35 FORMAT(1H ,
     &'Type: h all   - for a more detailed help directory')
   40 FORMAT(1H ,
     &'Type: h code  - for command list for associated subject')
C
      RETURN
      END
C$PROG CMPDEDX   - DEDX command processor for program STOPI
C
      SUBROUTINE CMPDEDX(IDONE,IERR)
C
      CHARACTER*4 IDONE
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
      COMMON/HEP/  IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/ST01/ KSORBSAV,KINTARG,LEM,NLST,ITPF,AP,ZP,AT,ZT,EA,EB,DE
      INTEGER*4    NLST,ITPF
      CHARACTER*4  KSORBSAV,KINTARG,LEM
      REAL*4                                      AP,ZP,AT,ZT,EA,EB,DE
C
      DATA         KSORBSAV/'SOLI'/
      DATA         KINTARG/'    '/
      DATA         LEM    /'LMON'/
      DATA         NLST   /1/
      DATA         ITPF   /'0C202020'X/
      DATA         AP     /4.0/
      DATA         ZP     /2.0/
      DATA         AT     /27.0/
      DATA         ZT     /13.0/
      DATA         EA     /0.0/
      DATA         EB     /0.0/
      DATA         DE     /0.0/
C     ------------------------------------------------------------------
      COMMON/ST02/ KSORB
      CHARACTER*4  KSORB
C
      DATA         KSORB  /'SOLI'/
C     ------------------------------------------------------------------
      INTEGER*4    NAMCMD(20),IHELP(20,400)
C
      INTEGER*4    LHEP
C
      CHARACTER*320 CLWD
C
      CHARACTER*4  KMD,KMX,MODE
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(1,1)),(CLWD,LWD)
C     ------------------------------------------------------------------
C     AP = PROJ (ION) MASS (AMU)
C     ZP = PROJ (ION) ATOMIC NO.
C     AT = TARGET MASS (AMU)
C     ZT = TARGET ATOMIC NO.
C     ------------------------------------------------------------------
C
      LHEP=13
C
C
C     ****************************************************************
C     READ IN FREE-FORM LINE AND DECODE
C     ****************************************************************
C
C
      IF(KMD.EQ.'H   ') GO TO 100
      IF(KMD.EQ.'CMD ') GO TO 105
C
      IF(KMD.EQ.'LMON') GO TO 110
      IF(KMD.EQ.'LMOF') GO TO 110
C
      IF(KMD.EQ.'LON ') GO TO 115
      IF(KMD.EQ.'LOF ') GO TO 115

      IF(KMD.EQ.'DEF ') GO TO 120
C
      IF(KMD.EQ.'SOLI') GO TO 130
      IF(KMD.EQ.'GAS ') GO TO 130
C
      IF(KMD.EQ.'PROJ') GO TO 140
      IF(KMD.EQ.'TARG') GO TO 150
      IF(KMD.EQ.'EION') GO TO 160
C
      IF(KMD.EQ.'DEDX') GO TO 200
      IF(KMD.EQ.'END ') GO TO 400
      GO TO 500
C
  100 CALL HELPMANU(IWD,LHEP,IHELP,400,20,IHEPF)
      GO TO 2500
C
  105 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)  !OPEN COMMAND FILE
      GO TO 2500
C
  110 LEM=KMD
      GO TO 2500
C
  115 LISFLG=KMD
      GO TO 2500
C
  120 MODE='DISP'
      CALL DOCMAN(MODE,6)
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Define form of absorber - SOLID, GAS, etc
C     ------------------------------------------------------------------
C
  130 KSORBSAV=KMD
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Get projectile
C     ------------------------------------------------------------------
C
  140 ILO=NXNB(IWD,5,80)
      IHI=NXBL(IWD,ILO,80)
      CALL GETAZ(IWD,ILO,IHI,JA,JZ,IERR)
      IF(IERR.NE.0) GO TO 500
      ZP=JZ
      AP=JA
      IF(JA.LE.0) AP=ANAT(JZ)
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Get target
C     ------------------------------------------------------------------
C
  150 CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0) GO TO 500
C
      KINTARG='    '
      ILO=NXNB(IWD,5,80)
      IHI=NXBL(IWD,ILO,80)
      CALL GETAZ(IWD,ILO,IHI,JA,JZ,IERR)
      IF(IERR.NE.0) GO TO 500
      ZT=JZ
      AT=JA
      IF(JA.LE.0) AT=ANAT(JZ)
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Get energy range, step, etc
C     ------------------------------------------------------------------
C
  160 CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      READ(CLWD,165)EA,EB,DE
  165 FORMAT(3F8.0)
      GO TO 2500
C
C     ****************************************************************
C     Process the DEDX request
C     ****************************************************************
C
  200 CALL DODEDX
      GO TO 2500
C
C
  400 IF(NLST.LE.0) STOP
C
      WRITE(LOGUP,405)ITPF
  405 FORMAT(A4,/1H ,'DEFINITIONS FOR PROGRAM STOPI RESULTS'//)
      MODE='LIST'
      CALL DOCMAN(MODE,7)
      STOP
C
  500 WRITE(6,505)
  505 FORMAT(1H ,'SYNTAX ERROR')
      GO TO 2500
C
 2500 IDONE='YES '
C
      RETURN
      END
C$PROG DODEDX    - Executes DEDX command for program STOPI
C
C     ******************************************************************
C     PROGRAM TO COMPUTE S(ELE+NUC) FROM ZIEGLER
C     ******************************************************************
C
      SUBROUTINE DODEDX
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/ST01/ KSORBSAV,KINTARG,LEM,NLST,ITPF,AP,ZP,AT,ZT,EA,EB,DE
      INTEGER*4    NLST,ITPF
      CHARACTER*4  KSORBSAV,KINTARG,LEM
      REAL*4                                      AP,ZP,AT,ZT,EA,EB,DE
C     ------------------------------------------------------------------
      COMMON/ST02/ KSORB
      CHARACTER*4  KSORB
C     ------------------------------------------------------------------
      REAL*4       ELST(40)
C
      CHARACTER*60 CHELP(21)
C
      CHARACTER*4  KFLG
C
      CHARACTER*53 CTILS
C
      INTEGER*4    TITLS(13)
C
      EQUIVALENCE (CTILS,TITLS)
C
      DATA (ELST(I),I=1,38)/
     1 .01,.02,.03,.04,.05,.06,.07,.08,.09,.10,
     2 .20,.30,.40,.50,.60,.70,.80,.90,1.0,2.0,
     3 3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.,20.,30.,
     4 40.,50.,60.,70.,80.,90.,100.,200./
C
C     ------------------------------------------------------------------
C     AP = PROJ (ION) MASS (AMU)
C     ZP = PROJ (ION) ATOMIC NO.
C     AT = TARGET MASS (AMU)
C     ZT = TARGET ATOMIC NO.
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
C     NOW DO THE LOOP ON ENERGY
C     ------------------------------------------------------------------
C
      NBLKS=0
      NBL=0
      WRITE(LOGUT,125)
  125 FORMAT(1H ,'    E(MEV)   MEV/AMU S(MEV.CMSQ/MG)  S(EV/(10**15',
     &' ATOMS/SQCM))')
C
      WRITE(LOGUP,130)ITPF
      WRITE(LOGUP,132)
C
  130 FORMAT(A4/1H ,'STOPPING POWERS CALCULATED (* * USING SOME',
     &' SHORT-CUTS) USING FORMULAS GIVEN BY J. F. ZIEGLER IN:')
C
  132 FORMAT(1H ,'THE STOPPING AND RANGES OF IONS IN MATTER,',
     &' VOLS 3 & 5, PERGAMON PRESS, 1980.')
C
      WRITE(LOGUP,140)IWD,ZP,AP,ZT,AT,EA,EB,DE
C
  140 FORMAT(1H ,20A4/1H ,'ZP,AP,ZT,AT,EA,EB,DE =',7F10.3/)
C
      WRITE(LOGUP,150)
      WRITE(LOGUP,152)
C
  150 FORMAT(1H ,'    E(MEV)   MEV/AMU           SNUC      SE(EFF-Q)',
     &'    SE(HE-FORM)    STOT(EFF-Q)  STOT(HE-FORM)    SE(HE-FORM)/')
C
  152 FORMAT(1H ,'                        MEV.CMSQ/MG    MEV.CMSQ/MG',
     &'    MEV.CMSQ/MG    MEV.CMSQ/MG    MEV.CMSQ/MG    SE(EFF-Q)'/)
C
      KINDE=1
      IF(EB.GT.EA.AND.DE.GT.0.0) GO TO 154
      KINDE=2
      NE=37
      NLO=1
      EMEV=0.01*AP
      GO TO 156
C
  154 NE=(EB-EA)/DE+1.0
      NLO=1
      EMEV=EA
C
  156 DO 200 I=NLO,NE
      EKEV=1000.0*EMEV
      CALL STOPUM(AP,ZP,AT,ZT,EKEV,SNEV,SEEV,STEV,SEEVHE,STEVHE)
      F=0.602/AT
      SN=F*SNEV
      SE=F*SEEV
      ST=F*STEV
      SEHE=0.0
      STHE=0.0
      SER=0.0
      EPN=EMEV/AP
      KFLG='    '
      IF(EPN.LT.0.5) KFLG='????'
      IF(EPN.LE.1.5) GO TO 158
C
      SEHE=F*SEEVHE
      STHE=F*STEVHE
      IF(SE.GT.0.0) SER=SEHE/SE
C
  158 WRITE(LOGUP,160)EMEV,EPN,SN,SE,SEHE,ST,STHE,SER,KFLG
C
      WRITE(LOGUT,162)EMEV,EPN,ST,STEV,KFLG
C
  160 FORMAT(1H ,2F10.4,6F15.5,2X,A4)
  162 FORMAT(1H ,F10.2,F10.4,2F15.5,2X,A4)
C
      NBL=NBL+1
      IF(NBL.LT.5) GO TO 190
      IF(I.GE.NE) GO TO 190
      NBL=0
      WRITE(LOGUP,170)
      WRITE(LOGUT,170)
C
  170 FORMAT(1H )
      NBLKS=NBLKS+1
      IF(NBLKS.LT.9) GO TO 190
C
      WRITE(LOGUP,130)
      WRITE(LOGUP,132)
      WRITE(LOGUP,140)IWD,ZP,AP,ZT,AT,EA,EB,DE
      WRITE(LOGUP,150)
      WRITE(LOGUP,152)
      NBLKS=0
      NBL=0
C
  190 IF(KINDE.EQ.1) EMEV=EMEV+DE
      IF(KINDE.EQ.2) EMEV=ELST(I+1)*AP
C
  200 CONTINUE
C
      RETURN
C
      END
