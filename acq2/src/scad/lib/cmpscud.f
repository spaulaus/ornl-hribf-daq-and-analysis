C$PROG CMPSCUD   - Command processor for SCUD
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/08/2004
C     ******************************************************************
C
      SUBROUTINE CMPSCUD(IDONE,IERR)
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
      COMMON/III/  LIN,LCM,LCI,LOU
      INTEGER*4    LIN,LCM,LCI,LOU
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DD03/ NCALLR
      INTEGER*4    NCALLR
C     ==================================================================
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                                      !STAR-8
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                                    ISOPEN
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
      INTEGER*4    GCOR,   GCON                                  !STAR-8
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL,    IBANNER
C     ==================================================================
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD2/ MSPRED,DISPSEC
      INTEGER*4    MSPRED,DISPSEC
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C     ==================================================================
      CHARACTER*4  KMD,KMX,KMM,IDONE,JDONE
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(1,2))
C     ==================================================================
C
      INTEGER*4    NAMCMD(20),IERR
C
      INTEGER*4    KIND,IV,NDX,LU,I
C
      REAL*4       XV
C
      CHARACTER*80 CWD
C
      DATA         LU/1/
C     ==================================================================
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'DLIN') GO TO 80
      IF(KMD.EQ.'DLOG') GO TO 85
C
      IF(KMD.EQ.'GLIN') GO TO 90
      IF(KMD.EQ.'GLOG') GO TO 90
C
      IF(KMD.EQ.'STAT') GO TO 120
      IF(KMD.EQ.'CMD ') GO TO 130
C
      IF(KMD.EQ.'FIG ') GO TO 220
      IF(KMD.EQ.'CMAP') GO TO 230
      IF(KMD.EQ.'REVV') GO TO 260
C
      IF(KMD.EQ.'RAV1') GO TO 310
      IF(KMD.EQ.'RAV2') GO TO 310
      IF(KMD.EQ.'RAV3') GO TO 310
      IF(KMD.EQ.'RAV4') GO TO 310
      IF(KMD.EQ.'RAV5') GO TO 310
      IF(KMD.EQ.'RAV6') GO TO 310
      IF(KMD.EQ.'RAV7') GO TO 310
      IF(KMD.EQ.'RAV8') GO TO 310
      IF(KMD.EQ.'RAV ') GO TO 320
C
      IF(KMD.EQ.'RAT1') GO TO 350
      IF(KMD.EQ.'RAT2') GO TO 350
      IF(KMD.EQ.'RAT3') GO TO 350
      IF(KMD.EQ.'RAT4') GO TO 350
      IF(KMD.EQ.'RAT5') GO TO 350
      IF(KMD.EQ.'RAT6') GO TO 350
      IF(KMD.EQ.'RAT7') GO TO 350
      IF(KMD.EQ.'RAT8') GO TO 350
C
      IF(KMD.EQ.'DPS ') GO TO 400
C
      IF(KMD.EQ.'GNIT') GO TO 500
      IF(KMD.EQ.'CLRG') GO TO 550
      IF(KMD.EQ.'CLRD') GO TO 550
C
      RETURN
C
   80 KMM='GLIN'
      GO TO 100
   85 KMM='GLOG'
      GO TO 100
   90 KMM=KMD
C
  100 DISPTYP=KMM
      CALL DOMETER('INIT',0,0.0)
      GO TO 2500
C
  120 CALL STATMAN
      GO TO 2500
C
  130 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)  !OPEN COMMAND FILE
      GO TO 2500
C
C
C     ------------------------------------------------------------------
C     Process FIG related commands
C     ------------------------------------------------------------------
C
  220 IF(NSCA.LE.0) GO TO 1330
      WRITE(CWD,225)NSCA
  225 FORMAT('FIG ',I1)
      CALL NEWFIG(LU,CWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL DOMETER('INIT',0,0.0)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NCALLR   =0
      GO TO 2500
C
  230 CALL COLRSET(10,IWD,IERR)
      GO TO 2500
C
  260 CALL REVV
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process RATE average related commands
C     ------------------------------------------------------------------
C
  310 NDX=1
      IF(KMD.EQ.'RAV2') NDX=2
      IF(KMD.EQ.'RAV3') NDX=3
      IF(KMD.EQ.'RAV4') NDX=4
      IF(KMD.EQ.'RAV5') NDX=5
      IF(KMD.EQ.'RAV6') NDX=6
      IF(KMD.EQ.'RAV7') NDX=7
      IF(KMD.EQ.'RAV8') NDX=8
C
      IF(NF.EQ.1) THEN
      RATAVG(NDX)=5
      GO TO 2500
      ENDIF
C
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2210
      IF(IV.GT.100) GO TO 2220
      IF(IV.EQ.0) IV=5
      RATAVG(NDX)=IV
      GO TO 2500
C
  320 IF(NF.EQ.1) THEN
      DO 325 I=1,NRATE
      RATAVG(I)=5
  325 CONTINUE
      GO TO 2500
      ENDIF
C
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2210
      IF(IV.GT.100) GO TO 2220
      IF(IV.EQ.0) IV=5
C
      DO 330 I=1,NRATE
      RATAVG(I)=IV
  330 CONTINUE
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process RATE ON/OFF commands
C     ------------------------------------------------------------------
C
  350 NDX=1
      IF(KMD.EQ.'RAT2') NDX=2
      IF(KMD.EQ.'RAT3') NDX=3
      IF(KMD.EQ.'RAT4') NDX=4
      IF(KMD.EQ.'RAT5') NDX=5
      IF(KMD.EQ.'RAT6') NDX=6
      IF(KMD.EQ.'RAT7') NDX=7
      IF(KMD.EQ.'RAT8') NDX=8
C
      IF(KMX.EQ.'OFF ') THEN
      RATTYP(NDX)=KMX
      RATPAR(NDX)=0
      GO TO 2500
      ENDIF
C
      IF(KMX.EQ.'ON  ') THEN
      RATTYP(NDX)='SCAL'
      RATPAR(NDX)=NDX
      GO TO 2500
      ENDIF
C
      GO TO 1320
C
C     ------------------------------------------------------------------
C     Process Displays/sec command - DPS
C     ------------------------------------------------------------------
C
  400 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2210
      IF(IV.GT.20)  GO TO 2230
C
      IF(IV.LE.0) IV=10
C
      DISPSEC=IV
      MSPRED=1000/DISPSEC
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Open & process scaler specification file - snit-file
C     ------------------------------------------------------------------
C
  500 CALL GNITTER(IERR)
      IF(IERR.NE.0) GO TO 2500
C
      NRATE=NSCA
C
      WRITE(CWD,505)NSCA
  505 FORMAT('FIG ',I1)
C
      CALL NEWFIG(LU,CWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL DOMETER('INIT',0,0.0)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NCALLR   =0
C
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Clear all meter display windows
C     ------------------------------------------------------------------
C
  550 DO 560 I=1,9
      IF(WINFLG(1,I).EQ.0) GO TO 560
      CALL XX_WINMAN('ERAS',I)
      WINFLG(1,I)=0
  560 CONTINUE
      NCALLR=0
      NRATE=0
      NSCA=0
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Generate error messages
C     ------------------------------------------------------------------
C
 1290 WRITE(CMSSG,1295)
 1295 FORMAT('Requested RAV outside allowed range of 0 to 100')
      GO TO 2400
C
 1300 WRITE(CMSSG,1305)NSCA
 1305 FORMAT('Requested RATE PAR outside allowed range of 1 to ',I4)
      GO TO 2400
C
 1310 WRITE(CMSSG,1315)NSCA
 1315 FORMAT('Requested scaler# outside allowed range of 0 to ',I4)
      GO TO 2400
C
 1320 WRITE(CMSSG,1325)KMX
 1325 FORMAT(A4,' is an illegal rate specifier')
      GO TO 2400
C
 1330 WRITE(CMSSG,1335)
 1335 FORMAT('Cannot FIG - Scalers have not been defined')
      GO TO 2400
C
 2210 WRITE(CMSSG,2215)
 2215 FORMAT('Illegal command or syntax error - cmd Ignored')
      GO TO 2400
C
 2220 WRITE(CMSSG,2225)IV
 2225 FORMAT('Specified value',I6,'  .GT. max value of 100 - ignored')
      GO TO 2400
C
 2230 WRITE(CMSSG,2235)IV
 2235 FORMAT('Specified value',I6,'  .GT. max value of 20 - ignored')
      GO TO 2400
C
 2400 CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2500
C
C     ------------------------------------------------------------------
C     RETURN
C     ------------------------------------------------------------------
C
 2500 IDONE='YES '
      RETURN
      END
