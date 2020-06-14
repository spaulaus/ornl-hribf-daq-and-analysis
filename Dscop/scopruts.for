C$PROG ABORTUM   - Closes EPICS connection and EXITs
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 03/16/2000
C     ******************************************************************
C
      SUBROUTINE ABORTUM
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/EP01/ BLNAME,EPOPEN
      CHARACTER*5  BLNAME,EPOPEN
C     ------------------------------------------------------------------
      CHARACTER*128 ERRMSG
C
      INTEGER*4     IERR
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
CX    IF(EPOPEN.EQ.'YES') CALL FCLOSE_EPICS(IERR,ERRMSG)
C
CX    IF(IERR.NE.0) WRITE(6,10)ERRMSG
CX 10 FORMAT(' ',A)
C
      CALL EXIT(0)
C
      END
C$PROG CMPSCOP   - Command processor for SCOP
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED r01r90/r131/200r21
C     ******************************************************************
C
      SUBROUTINE CMPSCOP(IDONE,IERR)
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
      COMMON/HEP/  IHEPF
      INTEGER*4    IHEPF
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
      INTEGER*4                  WINFLG,      NUMWIN,ISOPEN
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
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LA(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LA,      NSCA
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
      INTEGER*4    NAMCMD(20),IHELP(20,300),IERR,LHEP
C
      INTEGER*4    KIND,IV,NDX,LU,I
C
      REAL*4       XV
C
      CHARACTER*80 CWD
C
      DATA         LHEP/8/
      DATA         LU/1/
C     ==================================================================
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'ZERO') GO TO 10
C
      IF(KMD.EQ.'DLIN') GO TO 20
      IF(KMD.EQ.'DLOG') GO TO 25
C
      IF(KMD.EQ.'GLIN') GO TO 30
      IF(KMD.EQ.'GLOG') GO TO 30
C
      IF(KMD.EQ.'DOFF') GO TO 70
      IF(KMD.EQ.'DOF ') GO TO 70
C
      IF(KMD.EQ.'H   ') GO TO 100
      IF(KMD.EQ.'LON ') GO TO 110
      IF(KMD.EQ.'LOF ') GO TO 110
      IF(KMD.EQ.'STAT') GO TO 120
      IF(KMD.EQ.'CMD ') GO TO 130
      IF(KMD.EQ.'RUN ') GO TO 140
      IF(KMD.EQ.'TST ') GO TO 140
      IF(KMD.EQ.'END ') GO TO 150
C
      IF(KMD.EQ.'GLIM') GO TO 200
      IF(KMD.EQ.'RLIM') GO TO 200
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
      IF(KMD.EQ.'SNIT') GO TO 500
C
      RETURN
C
   10 CALL CAMSYM('ZERO')
      CALL DISPRATE('ZERO')
      GO TO 2500
C
   20 KMM='GLIN'
      GO TO 40
   25 KMM='GLOG'
      GO TO 40
   30 KMM=KMD
C
   40 IF(DISPTYP.EQ.'DOFF') GO TO 50
C
      DISPTYP=KMM
      CALL DOMETER('INIT',0,0.0)
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Re-init things after local display has been disabled
C     ------------------------------------------------------------------
C
   50 DISPTYP=KMM
      GO TO 220
C
C     ------------------------------------------------------------------
C     Turn off local display & clear all windows
C     ------------------------------------------------------------------
C
   70 DO 80 I=1,9
      IF(WINFLG(1,I).EQ.0) GO TO 80
      CALL XX_WINMAN('ERAS',I)
      WINFLG(1,I)=0
   80 CONTINUE
      NCALLR=0
      DISPTYP='DOFF'
      GO TO 2500
C
  100 CALL HELPMANU(IWD,LHEP,IHELP,300,20,IHEPF)
      GO TO 2500
C
  110 LISFLG=KMD
      GO TO 2500
C
  120 CALL STATMAN
      GO TO 2500
C
  130 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)  !OPEN COMMAND FILE
      GO TO 2500
C
  140 CALL DISPLAR(KMD)
      GO TO 2500
C
  150 CALL ABORTUM
C
C     ------------------------------------------------------------------
C     Set Rate-Limits
C     ------------------------------------------------------------------
C
  200 CALL GLIMSET
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process FIG related commands
C     ------------------------------------------------------------------
C
  220 IF(NSCA.LE.0)         GO TO 1330
      IF(DISPTYP.EQ.'DOFF') GO TO 1340
C
      WRITE(CWD,225)NSCA
  225 FORMAT('FIG ',I1)
      CALL NEWFIG(LU,CWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL DOMETER('INIT',0,0.0)
C
      CALL WINRAT
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
      CALL WINRAT
      GO TO 2500
      ENDIF
C
      IF(KMX.EQ.'ON  ') THEN
      RATTYP(NDX)='SCAL'
      RATPAR(NDX)=NDX
      CALL WINRAT
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
  500 CALL SNITTER(IERR)
      IF(IERR.NE.0) GO TO 2500
C
      NRATE=NSCA
C
      IF(DISPTYP.EQ.'DOFF') GO TO 2500
C
      WRITE(CWD,505)NSCA
  505 FORMAT('FIG ',I1)
C
      CALL NEWFIG(LU,CWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL DOMETER('INIT',0,0.0)
C
      CALL WINRAT
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NCALLR   =0
C
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
 1335 FORMAT('Cannot FIG now - Scalers have not been defined')
      GO TO 2400
C
 1340 WRITE(CMSSG,1345)
 1345 FORMAT('Cannot FIG now - Display is currently turned OFF')
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
      IERR=1
      GO TO 2500
C
C     ------------------------------------------------------------------
C     RETURN
C     ------------------------------------------------------------------
C
 2500 IDONE='YES '
      RETURN
      END
C$PROG DISPRATE  - Displays RATE in meter form
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 03/16/2000
C     ******************************************************************
C
      SUBROUTINE DISPRATE(MODE)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ==================================================================
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                                      !STAR-8
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/XLBB/ AA(2,20),BB(2,20),PLOTYP(20)
      REAL*4       AA,      BB
      CHARACTER*4                    PLOTYP
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      INTEGER*4    WINDAT,       WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
      INTEGER*4    GCOR,   GCON                                  !STAR-8
C     ------------------------------------------------------------------
      COMMON/DD03/ NCALLR
      INTEGER*4    NCALLR
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD12/ NRATSCA(16)
      INTEGER*4    NRATSCA
C     ------------------------------------------------------------------
      COMMON/DD15/ NCALLRI
      INTEGER*4    NCALLRI
      DATA         NCALLRI/0/
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C     ==================================================================
      CHARACTER*4  MODE
C
      CHARACTER*128 ERRMSG
C
      INTEGER*4   NWIN,NLAS,NDO,IERR,NERR,I,J
C
      INTEGER*4   NRAT(9),IRAT(9),ID1
C
      REAL*4      RATLIS(120,9),RATE(9),RAT(9),SUM(9)
C
      REAL*4      SECVLU,TLAST,TNOW,DELT
C
      DATA        NWIN/1/
      DATA        NLAS/0/
      DATA        NERR/0/
      DATA        RATE/9*1.0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(MODE.EQ.'ZERO') THEN
      DO 15 J=1,9
      IRAT(J)=0
      DO 10 I=1,120
      RATLIS(I,J)=0
   10 CONTINUE
   15 CONTINUE
      RETURN
      ENDIF
C
C
      IF(RATDSP.EQ.'LOG ') GO TO 20
C
      IF(NCALLRI.GT.0)     GO TO 20
C
C     ------------------------------------------------------------------
C     Init range indicators and time on first call
C     ------------------------------------------------------------------
C
      TLAST=SECVLU(0.0)
      NCALLRI=1
      RETURN
C
C     ------------------------------------------------------------------
C     Compute time & erase last display if appropriate ??
C     ------------------------------------------------------------------
C
   20 TNOW=SECVLU(0.0)
      DELT=TNOW-TLAST
      TLAST=TNOW
C
      IF(NCALLR.EQ.0) GO TO 30
      IF(NLAS.LE.0)   GO TO 30
C
   30 IF(RATFLG.NE.'RON ') THEN
      NLAS=0
      RETURN
      ENDIF
C
C     ------------------------------------------------------------------
C     Compute rates, add to rate-lists, do averaging, etc
C     ------------------------------------------------------------------
C
      DO 40 I=1,NRATE
C
      IF(RATTYP(I).EQ.'SCAL') THEN
      ID1=RATPAR(I)
      RATE(I)=FLOAT(NRATSCA(ID1))
      GO TO 40
      ENDIF
C
      RATE(I)=0.0
C
   40 CONTINUE
C
C
      DO 80 I=1,NRATE
      NRAT(I)=RATAVG(I)
      IF(RATE(I).LT.0.0) GO TO 80
      IRAT(I)=IRAT(I)+1
      IF(IRAT(I).GT.NRAT(I)) IRAT(I)=1
      RATLIS(IRAT(I),I)=RATE(I)
   80 CONTINUE
C
      DO 100 J=1,NRATE
      SUM(J)=0
      NDO=NRAT(J)
      DO 90 I=1,NDO
      SUM(J)=SUM(J)+RATLIS(I,J)
   90 CONTINUE
      RATE(J)=SUM(J)/FLOAT(NRAT(J))
  100 CONTINUE
C
C     ------------------------------------------------------------------
C     Do METER display & send rate-data to EPICS
C     ------------------------------------------------------------------
C
  500 IF(DISPTYP.EQ.'DOFF') GO TO 600
C
      DO 520 I=1,NRATE
C
      CALL DOMETER('RATE',I,RATE(I))
C
  520 CONTINUE
C
  600 CONTINUE
C
CX    CALL FWRITE_EPICS(NRATE,RATE,IERR,ERRMSG)
C
CX    IF(IERR.NE.0) THEN
CX    NERR=NERR+1
CX    WRITE(6,530)ERRMSG
CX530 FORMAT(' ',A)
CX    ENDIF
C
CX    IF(NERR.GE.100) CALL ABORTUM
C
      NLAS=NDO
C
      NCALLR=1
C
      NCALLRI=0
C
      RETURN
      END
C$PROG DOMETER   - Draws scaler display "meter face"
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/15/2002
C     ******************************************************************
C
      SUBROUTINE DOMETER(MODE,IDW,COUNT)
C
      INTEGER*4    DPY,WDID                             !STAR-8 on Alpha
C
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
C
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
      INTEGER*4    GCOR,   GCON                         !STAR-8 on Alpha
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      CHARACTER*80 CTITL
      EQUIVALENCE (CTITL,ITITL)
C     ------------------------------------------------------------------
      COMMON/ME00/SQSIZ(20),FACRAD(20),NUDSP(20),NUDEC(20)
      REAL*4      SQSIZ,    FACRAD
      INTEGER*4                        NUDSP,    NUDEC
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
C
      CHARACTER*4  MODE,NUFLG
C
      DATA         DTOR/0.017453292/
C
C     ------------------------------------------------------------------
C
      REAL*4       ALN,BLN,ALG,BLG,ADC,BDC
C     ------------------------------------------------------------------
      DATA         ALN,BLN/225.0,-22.5/         !Scaling derived from:
C                                               !ANG = A + B*CNT
C                                               !for CNT=0,  ANG=225 deg
C                                               !for CNT=12, ANG=-45 deg
C     ------------------------------------------------------------------
      DATA         ALG,BLG/225.0,-90.12828/     !Scaling derived from:
C                                               !ANG = A + B*ALOG(CNT)
C                                               !for CNT=1,  ANG=225 deg
C                                               !for CNT=20, ANG=-45 deg
C     ------------------------------------------------------------------
      DATA         ADC,BDC/225.0,-30.0/         !Scaling derived from:
C                                               !ANG = A + B*CNT
C                                               !for CNT=0,  ANG=225 deg
C                                               !for CNT=9,  ANG=-45 deg
C     ------------------------------------------------------------------
C
      INTEGER*2    XYP(2,3,20)
C
      REAL*8       CMIN(20),CMAX(20),CMUL(20),CNT
C
      DATA         CMIN,CMAX,CMUL/20*0.0,20*12.0,20*1.0/
C
      CHARACTER*8  ERRFLG(20),LASFLG(20)
C
      INTEGER*4    ITST
C
      INTEGER*4    JXX,JYY,NCL(20),NCALL(20)
C
      DATA         NCL,NCALL/20*0,20*0/
C
      CHARACTER*10 CCNT,LASCCNT(20)
C
      INTEGER*4    ITOG(20)
      DATA         ITOG/20*2/
C
      INTEGER*4    LGCL(2),LGCH(2),LGCZ
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(MODE.EQ.'RATE') GO TO 100
C
      IF(MODE.NE.'INIT') RETURN
C
      DO 20 N=1,NUMWIN
C
      NUDSP(N)=0
      NUDEC(N)=0
C
      CMIN(N) =0.8
      CMAX(N) =12.0
      CMUL(N) =1.0
C
      IF(DISPTYP.EQ.'GLOG') THEN
      CMIN(N)=1.0
      CMAX(N)=20.0
      ENDIF
C
      ERRFLG(N)=' '
      LASFLG(N)=' '
C
      CALL NUMETER(N,CMUL(N),ERRFLG(N))
C
      NCALL(N)=0
C
      NCL(N)=0
C
   20 CONTINUE
C
      CALL GETGCO('WHIT',LGCL(1))
      CALL GETGCO('ERAS',LGCL(2))
C
      CALL GETGCO('RDGR',LGCH(1))
      CALL GETGCO('ERAS',LGCH(2))
C
      CALL GETGCO('ERAS',LGCZ)
C
      CALL GETGCO('OWHI',LGC)
C
      RETURN
C
C
C     ------------------------------------------------------------------
C     Display rates
C     ------------------------------------------------------------------
C
  100 N=IDW
C
      NUFLG='NO  '
C
      CNT=COUNT
C
      IF(CNT.LT.0.0) CNT=0.0
C
      ERRFLG(N)=' '
C
      IF(DISPTYP.EQ.'GLIN')  GO TO 300
C
C     ------------------------------------------------------------------
C     Do it for LOG display
C     ------------------------------------------------------------------
C
      IF(CNT.LT.1.0) THEN
      CMUL(N)=1.0
      CMIN(N)=1.0
      CMAX(N)=20.0
      CNT=1.0
      IF(LASFLG(N).EQ.' ') NUFLG='YES '
      ERRFLG(N)='  cps<1'
      GO TO 500
      ENDIF
C
  200 IF(CNT.GE.CMIN(N).AND.CNT.LE.CMAX(N)) THEN
      ERRFLG(N)=' '
      IF(LASFLG(N).NE.' ') NUFLG='YES '
      GO TO 500
      ENDIF
C
      IF(CNT.GT.CMAX(N)) THEN
      CMUL(N)=0.10*CMUL(N)
      CMIN(N)=10.0*CMIN(N)
      CMAX(N)=10.0*CMAX(N)
      NUFLG='YES '
      GO TO 200
      ENDIF
C
      IF(CNT.LT.CMIN(N)) THEN
      CMUL(N)=10.0*CMUL(N)
      CMIN(N)=0.10*CMIN(N)
      CMAX(N)=0.10*CMAX(N)
      NUFLG='YES '
      GO TO 200
      ENDIF
C
C     ------------------------------------------------------------------
C     DO it for LINEAR display
C     ------------------------------------------------------------------
C
  300 IF(CNT.GE.CMIN(N).AND.CNT.LE.CMAX(N)) GO TO 500
C
      ITST=1.0/CMUL(N)+0.5
C
      IF(CNT.LT.CMIN(N).AND.ITST.EQ.1) GO TO 500
C
      IF(CNT.GT.CMAX(N)) THEN
      CMUL(N)=0.10*CMUL(N)
      CMIN(N)=10.0*CMIN(N)
      CMAX(N)=10.0*CMAX(N)
      NUFLG='YES '
      GO TO 300
      ENDIF
C
      IF(CNT.LT.CMIN(N)) THEN
      CMUL(N)=10.0*CMUL(N)
      CMIN(N)=0.10*CMIN(N)
      CMAX(N)=0.10*CMAX(N)
      NUFLG='YES '
      GO TO 300
      ENDIF
C

  500 IF(NUFLG.NE.'YES ') GO TO 600
C
C
C     ------------------------------------------------------------------
C     Draw new banner & decade pointer
C     ------------------------------------------------------------------
C
      CALL NUMETER(N,CMUL(N),ERRFLG(N))
C
      NCALL(N)=0
C
      RP=12.0
C
      IMUL=1.0/CMUL(N)+0.5
C
      ANGL=ADC+BDC*ALOG10(FLOAT(IMUL))
C
      ARGP=DTOR*(ANGL+90.0)
      ARGM=DTOR*(ANGL-90.0)
C
      PX1=RP*COS(ARGM)
      PY1=RP*SIN(ARGM)
C
      PX2=RP*COS(ARGP)
      PY2=RP*SIN(ARGP)
C
      N=IDW
C
      IF(NUDEC(N).NE.0) THEN
      CALL XX_FILLPOLYGON(DPY,WDID(N),GCOR(2),XYP(1,1,N),3,MODE_OR)
      CALL XX_SYNC(DPY,.TRUE.)
      ENDIF
C
      PX3=0.4*FACRAD(N)*COS(DTOR*ANGL)
      PY3=0.4*FACRAD(N)*SIN(DTOR*ANGL)
C
      XYP(1,1,N)=0.5*SQSIZ(N)+PX1+0.5
      XYP(2,1,N)=0.5*SQSIZ(N)-PY1+0.5
C
      XYP(1,2,N)=0.5*SQSIZ(N)+PX2+0.5
      XYP(2,2,N)=0.5*SQSIZ(N)-PY2+0.5
C
      XYP(1,3,N)=0.5*SQSIZ(N)+PX3+0.5
      XYP(2,3,N)=0.5*SQSIZ(N)-PY3+0.5
C
      CALL XX_FILLPOLYGON(DPY,WDID(N),GCOR(2),XYP(1,1,N),3,MODE_OR)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NUDEC(N)=1
C
C     ------------------------------------------------------------------
C     Draw count pointer
C     ------------------------------------------------------------------
C
  600 RP=0.6*FACRAD(N)
C
      IF(DISPTYP.EQ.'GLIN') ANGL=ALN+BLN*CMUL(N)*CNT
C
      IF(DISPTYP.EQ.'GLOG') ANGL=ALG+BLG*DLOG(CMUL(N)*CNT)
C
      ARGP=DTOR*(ANGL+12.0)
      ARGM=DTOR*(ANGL-12.0)
C
      PX1=RP*COS(ARGM)
      PY1=RP*SIN(ARGM)
C
      PX2=RP*COS(ARGP)
      PY2=RP*SIN(ARGP)
C
      N=IDW
C
      IF(NUDSP(N).NE.0) THEN
      CALL XX_FILLPOLYGON(DPY,WDID(N),GCOR(2),XYP(1,1,N),3,MODE_OR)
      CALL XX_SYNC(DPY,.TRUE.)
      ENDIF
C
      PX3=FACRAD(N)*COS(DTOR*ANGL)
      PY3=FACRAD(N)*SIN(DTOR*ANGL)
C
      XYP(1,1,N)=0.5*SQSIZ(N)+PX1+0.5
      XYP(2,1,N)=0.5*SQSIZ(N)-PY1+0.5
C
      XYP(1,2,N)=0.5*SQSIZ(N)+PX2+0.5
      XYP(2,2,N)=0.5*SQSIZ(N)-PY2+0.5
C
      XYP(1,3,N)=0.5*SQSIZ(N)+PX3+0.5
      XYP(2,3,N)=0.5*SQSIZ(N)-PY3+0.5
C
      CALL XX_FILLPOLYGON(DPY,WDID(N),GCOR(2),XYP(1,1,N),3,MODE_OR)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NUDSP(N)=1
C
      LASFLG(N)=ERRFLG(N)
C
C     ------------------------------------------------------------------
C     Display numeric rate in graphics window
C     ------------------------------------------------------------------
C
      NCL(N)=NCL(N)+1
C
      IF(NCL(N).GE.10)   THEN
C
      NCL(N)=0
      JYY=WINDAT(4,N)-8
      JXX=            90
C
      IF(NCALL(N).NE.0) THEN
      CALL XX_DRAWSTRING(DPY,WDID(N),LGC,JXX,JYY,LASCCNT(N))
      ENDIF
C
      WRITE(CCNT,605)CNT
  605 FORMAT(1PE10.2)
      CALL XX_DRAWSTRING(DPY,WDID(N),LGC,JXX,JYY,CCNT)
      LASCCNT(N)=CCNT
C
      IF(GLIMON(N).EQ.'ON  ') THEN
      ITOG(N)=3-ITOG(N)
      LGCX=LGCL(2)
      IF(CNT.LT.GLIMLO(N)) LGCX=LGCL(ITOG(N))
      IF(CNT.GT.GLIMHI(N)) LGCX=LGCH(ITOG(N))
C
      IF(NCALL(N).NE.0) THEN
      CALL FILLCIR(N,LGCZ,0,0,50,50)
      CALL FILLCIR(N,LGCZ,0,195,50,50)
      CALL FILLCIR(N,LGCZ,195,0,50,50)
      CALL FILLCIR(N,LGCZ,195,195,50,50)
      CALL XX_SYNC(DPY,.TRUE.)
      ENDIF
C
      IF(CNT.GT.GLIMHI(N)) THEN
      CALL FILLCIR(N,LGCX,0,0,50,50)
      CALL FILLCIR(N,LGCX,195,0,50,50)
      ENDIF
C
      IF(CNT.LT.GLIMLO(N)) THEN
      CALL FILLCIR(N,LGCX,0,195,50,50)
      CALL FILLCIR(N,LGCX,195,195,50,50)
      ENDIF
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      ENDIF
C
      NCALL(N)=1
C
      ENDIF
C
      RETURN
      END
C$PROG GLABNDX   - Searches for & returns LABEL index in current LIST
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      FUNCTION GLABNDX(LABL)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      INTEGER*4 LABL(3)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      DO 20 N=1,NSCA
      DO 10 I=1,3
      IF(LABL(I).NE.LAG(I,N)) GO TO 20
   10 CONTINUE
      GO TO 50
   20 CONTINUE
C
      GLABNDX=0
      RETURN
C
   50 GLABNDX=N
      RETURN
      END
C$PROG GLIMSET   - Sets up rate-limits for GRAPHIC display & test
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 12/01/2005
C     ******************************************************************
C
      SUBROUTINE GLIMSET
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C
      DATA         GLIMON/16*'NULL'/
      DATA         GLIMLO/16*0.0/
      DATA         GLIMHI/16*0.0/
C     ------------------------------------------------------------------
      INTEGER*4    LWD3(3,40),ITYP3(40),NF3,NTER3
C
      INTEGER*4    GLABNDX,NDX,IV,KIND,IERR,IT,N,I,J,IDX
C
      REAL*4       XLO,XHI
C
      CHARACTER*4  KMX,KMY
      EQUIVALENCE (KMX,LWD(1,2)),(KMY,LWD(1,3))
C
      INTEGER*4    SETW
      CHARACTER*4  CSETW
      EQUIVALENCE (CSETW,SETW)
      DATA         CSETW/'SETW'/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMX.EQ.'SHO ') GO TO 100
      IF(KMX.EQ.'OFF ') GO TO 200
      IF(KMX.EQ.'ON  ') GO TO 220
      IF(KMX.EQ.'NULL') GO TO 240
C
C
      CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,SETW,12,NTER3)
C
      CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,1,80,NTER3)
C
      IF(NTER3.NE.0)    GO TO 500
C
      NDX=GLABNDX(LWD3(1,2))
C
      IF(NDX.LE.0)     GO TO 510
C
C
      IF(KMY.EQ.'OFF ') THEN
      IF(GLIMON(NDX).EQ.'NULL') GO TO 540
      GLIMON(NDX)='OFF '
      GO TO 1000
      ENDIF
C
      IF(KMY.EQ.'ON  ') THEN
      IF(GLIMON(NDX).EQ.'NULL') GO TO 540
      GLIMON(NDX)='ON  '
      GO TO 1000
      ENDIF
C
      IF(KMY.EQ.'NULL') THEN
      GLIMON(NDX)='NULL'
      GLIMLO(NDX)=0.0
      GLIMHI(NDX)=0.0
      GO TO 1000
      ENDIF
C
      IF(NF3.NE.4) GO TO 500
C
      CALL MILV3(LWD3(1,3),IV,XLO,KIND,IERR)
C
      IF(IERR.NE.0)    GO TO 520
C
      CALL MILV3(LWD3(1,4),IV,XHI,KIND,IERR)
C
      IF(IERR.NE.0)    GO TO 530
C
      GLIMON(NDX)='ON  '
      GLIMLO(NDX)=XLO
      GLIMHI(NDX)=XHI
      GO TO 1000
C
C     ------------------------------------------------------------------
C     Display current limit table
C     ------------------------------------------------------------------
C
  100 DO 120 N=1,NSCA
      WRITE(6,105)(LAG(I,N),I=1,3),GLIMLO(N),GLIMHI(N),GLIMON(N)
  105 FORMAT(3A4,2F10.3,2X,A4)
  120 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Turn ON/OFF all limits
C     ------------------------------------------------------------------
C
  200 DO 210 I=1,NSCA
      IF(GLIMON(I).NE.'NULL') GLIMON(I)='OFF '
  210 CONTINUE
      RETURN
C
  220 DO 230 I=1,NSCA
      IF(GLIMON(I).NE.'NULL') GLIMON(I)='ON  '
  230 CONTINUE
      RETURN
C
  240 DO 250 I=1,NSCA
      GLIMON(I)='NULL'
      GLIMLO(I)=0.0
      GLIMHI(I)=0.0
  250 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Report error messages
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error in graphic rate limit def - cmd ignored')
      GO TO 900
C
  510 WRITE(CMSSG,515)(LWD3(I,2),I=1,3)
  515 FORMAT('Undefined igraphic scaler label = ',3A4,' - cmd ignored')
      GO TO 900
C
  520 WRITE(CMSSG,525)
  525 FORMAT('Syntax error in graphic rate lo-limit def - cmd ignored')
      GO TO 900
C
  530 WRITE(CMSSG,535)
  535 FORMAT('Syntax error in graphic rate hi-limit def - cmd ignored')
      GO TO 900
C
  540 WRITE(CMSSG,545)
  545 FORMAT('Specified limit is NULL - cmd ignored')
C
  900 CALL MESSLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,SETW,8,NTER3)
      RETURN
      END
C$PROG SNITTER   - Opens & processes scaler init file
C
C     ******************************************************************
C     BY W.T. MILNER AT ORPH - LAST MODIFIED 03/23/2000
C     ******************************************************************
C
      SUBROUTINE SNITTER(IERR)
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LA(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LA,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD1/ SNITNAM
      CHARACTER*80 SNITNAM
C     ------------------------------------------------------------------
      COMMON/EP01/ BLNAME,EPOPEN
      CHARACTER*5  BLNAME,EPOPEN
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
      CHARACTER*128  ERRMSG
C
      INTEGER*4    IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      INTEGER*4    STRLEN,NXBL,NXNB
C
      INTEGER*4    IERR,JERR,KIND,ISTAT,NLN,IA,I,J,N
C
      INTEGER*4    IV(4)
C
      REAL*4       XV
C
      CHARACTER*80 CWDRAW,CNAM
C
      CHARACTER*4  CWD(20)
C
      EQUIVALENCE (CWDRAW,IWDRAW),(CWD,IWD)
C
      CHARACTER*1  COMBYT
C
      EQUIVALENCE (COMBYT,IWD)
C
      INTEGER*4    MXSCA,LU
      DATA         MXSCA,LU/9,17/
C
      INTEGER*4    BLANK
      DATA         BLANK/'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     Pick up file name and open the mighyt snit-file
C     ------------------------------------------------------------------
C
      IERR=0
C
      IA=NXNB(IWDRAW,5,80)
      IF(IA.LE.0) GO TO 500
      CNAM=CWDRAW(IA:STRLEN(CWDRAW))
C
      CLOSE(UNIT=LU)
C
      OPEN(UNIT     = LU,
     &     FILE     = CNAM,
     &     STATUS   = 'OLD',
     &     IOSTAT   = ISTAT)
C
      IF(ISTAT.NE.0) GO TO 510
C
      SNITNAM=CNAM
C
C     ------------------------------------------------------------------
C     Initialize scaler labels for whatever reason
C     ------------------------------------------------------------------
C
      DO 20 J=1,MXSCA
      DO 10 I=1,3
      LA(I,J)=BLANK
   10 CONTINUE
      GLIMON(J)='NULL'
      GLIMLO(J)=0.0
      GLIMHI(J)=0.0
   20 CONTINUE
C
C     ------------------------------------------------------------------
C     Read and process the snit-file
C     ------------------------------------------------------------------
C
      N=0                                    !Init # of scalers
      NLN=0                                  !Init line# counter
C
   50 READ(LU,55,END=200)IWD
   55 FORMAT(20A4)
C
      NLN=NLN+1                              !Inc line# counter
C
      IF(CWD(1).EQ.' ')    GO TO 50          !Ignore blank lines
C
      IF(COMBYT.EQ.'#')    GO TO 50          !Ignore comment lines
C
      IF(CWD(1).EQ.'$END') GO TO 200
C
      IA=NXBL(IWD,1,80)                      !Locate label field
      IF(IA.GT.12)   GO TO 520               !Tst for error
      IF(IA.LE.0)    GO TO 520               !Tst for error
C
      N=N+1                                  !Inc # of scalers
      IF(N.GT.MXSCA) GO TO 540               !Tst for too many
C
      CALL LODUP(IWD,1,IA,LA(1,N),1)         !load up the label
C
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER) !Reformat CNAF fields
      IF(NTER.NE.0)  GO TO 520               !Tst for Error
C
      DO 60 I=1,4
      CALL MILV(LWD(1,I),IV(I),XV,KIND,JERR)
      IF(JERR.NE.0) GO TO 520
   60 CONTINUE
C
      CC(N)=IV(1)
      NN(N)=IV(2)
      AA(N)=IV(3)
      FF(N)=0
C
      GO TO 50                               !Go back for more
C
  200 CLOSE(LU)                              !Close snit-file
      NSCA=N                                 !Save # of scalers
      IF(NSCA.GT.8) NSCA=8                   !Limit to 8
      NRATE=NSCA                             !Save # of scalers
      WRITE(CMSSG,205)NSCA
  205 FORMAT(I3,' scalers set up')
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Call routine to open EPICS server
C     ------------------------------------------------------------------
C
CX    IF(EPOPEN.EQ.'YES') THEN
CX    CALL FCLOSE_EPICS(IERR,ERRMSG)
CX    EPOPEN='NO'
CX    ENDIF
C
CX    CALL FOPEN_EPICS(NSCA,BLNAME,IERR,ERRMSG)
C
CX    IF(IERR.NE.0) THEN
CX    WRITE(6,210)ERRMSG
CX210 FORMAT(' ',A)
CX    IERR=1
CX    RETURN
CX    ENDIF
C
      WRITE(6,250)BLNAME
  250 FORMAT(' Beamline is ',A)
C
      EPOPEN='YES'
C
      RETURN                                 !Return
C
C     ------------------------------------------------------------------
C     Error Returns
C     ------------------------------------------------------------------
C
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error in snit-file name')
      GO TO 1000
C
  510 WRITE(CMSSG,515)CNAM
  515 FORMAT('Unable to open snit-file - ',A)
      GO TO 1000
C
  520 WRITE(CMSSG,525)NLN
  525 FORMAT('Syntax error on snit-file line# = ',I3)
      GO TO 1000
C
  540 WRITE(CMSSG,545)MXSCA,NLN
  545 FORMAT('More than max of ',I2,' scalers defined on line#',I3)
      GO TO 1000
C
C
 1000 IERR=1
      NSCA=0
      NRATE=0
      SNITNAM='Undefined!'
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG STATMAN   - Status display routine for SCOP
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 11/10/99
C     ******************************************************************
C
      SUBROUTINE STATMAN
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
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LA(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LA,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD1/ SNITNAM
      CHARACTER*80 SNITNAM
C     ------------------------------------------------------------------
      COMMON/SCD2/ MSPRED,DISPSEC
      INTEGER*4    MSPRED,DISPSEC
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
      INTEGER*4    I,J,N
C
      CHARACTER*4  STATE
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     Display RATE related status
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,55)SNITNAM
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,56)DISPSEC
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
   50 FORMAT('===================================================',
     &       '=============')
C
   55 FORMAT('Snit-file name = ',A)
   56 FORMAT('Displays/sec   = ',I2)
C
      WRITE(CMSSG,60)
   60 FORMAT('NAME            C   N   A   F  NAGV  STATE')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 100 N=1,NSCA
C
      STATE=' OFF'
C
      IF(RATTYP(N).EQ.'SCAL') STATE='  ON'
C
      WRITE(CMSSG,70)(LA(I,N),I=1,3),CC(N),NN(N),AA(N),FF(N),
     &                RATAVG(N),STATE
   70 FORMAT(3A4,1X,4I4,I6,3X,A)
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 CONTINUE
C
C     ------------------------------------------------------------------
C     Display limit related status
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,110)
  110 FORMAT('Limit Specifications       LOW        HIGH')
C
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 120 J=1,NSCA
      WRITE(CMSSG,115)(LA(I,J),I=1,3),GLIMLO(J),GLIMHI(J),GLIMON(J)
  115 FORMAT(3A4,6X,2F12.1,2X,A4)
      CALL MESSLOG(LOGUT,LOGUP)
  120 CONTINUE
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
      RETURN
C
      END
