C$PROG CMPDEDX   - DEDX command processor for program STOPO
C
      SUBROUTINE CMPDEDX(IDONE,IERR)
C
      CHARACTER*4 IDONE
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
      DATA         NLST   /0/
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
      IF(KMX.EQ.'H2O ') THEN
      KINTARG=KMX
      ZT=13.0
      AT=27.0
      GO TO 2500
      ENDIF
C
      IF(KMX.EQ.'MYLA') THEN
      KINTARG=KMX
      ZT=13.0
      AT=27.0
      GO TO 2500
      ENDIF
C
      IF(KMX.EQ.'CH2 ') THEN
      KINTARG=KMX
      ZT=13.0
      AT=27.0
      GO TO 2500
      ENDIF
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
  405 FORMAT(A4,/1H ,'DEFINITIONS FOR PROGRAM STOPE RESULTS'//)
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
