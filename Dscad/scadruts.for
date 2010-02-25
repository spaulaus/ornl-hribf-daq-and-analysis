C$PROG CMPSCAD   - Command processor for program SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/22/2005
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SCALER DISPLAY PROGRAM - FOR CAMAC SCALERS
C     CAN USE TVI912C OR ANSI TERMINALS (DEPENDING ON START STRING)
C     ------------------------------------------------------------------
C
      SUBROUTINE CMPSCAD(IDONE,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      INTEGER*4    KTERM
C     ------------------------------------------------------------------
      COMMON/SD10/ MODEGO
      CHARACTER*4  MODEGO
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
      COMMON/SD13/ ISET,LULG
      CHARACTER*4  ISET
      INTEGER*4         LULG
C     ------------------------------------------------------------------
      COMMON/SD14/ MXBEEP,MXGOOD,BELL,BELLV,NBEEP
      INTEGER*4    MXBEEP,MXGOOD,BELL,BELLV,NBEEP
C     ------------------------------------------------------------------
      COMMON/SD15/ LUAL,THUSH,TNOW,ALFLG
      INTEGER*4    LUAL,THUSH,TNOW
      CHARACTER*4                  ALFLG
C     ------------------------------------------------------------------
      COMMON/SD16/ PRNAM
      CHARACTER*76 PRNAM
      DATA         PRNAM/' '/
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI,LOU
      INTEGER*4    LIN,LCM,LCI,LOU
C     ------------------------------------------------------------------
      INTEGER*4    IHELP(20,300),LHEP
C
      CHARACTER*20 MDATIM
C
      CHARACTER*4  CLWD(2,40),IDONE
      EQUIVALENCE (CLWD,LWD)
C
      INTEGER*4    NAMCMD(20),IERR,BLANK
C
      DATA         SEC,LSEC/5,0/
C
      DATA         KTERM,BLANK/'ANSI','    '/
C
      DATA         LHEP/16/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
C
      WRITE(6,777)KMY
  777 FORMAT(A)
C
      IERR=0
C
      KMD=CLWD(1,1)
C
      IF(KMD.EQ.'LON ') GO TO 100
      IF(KMD.EQ.'LOF ') GO TO 100
      IF(KMD.EQ.'CMD ') GO TO 110
      IF(KMD.EQ.'    ') GO TO 2000
      IF(KMD.EQ.'H   ') GO TO 125
      IF(KMD.EQ.'HELP') GO TO 125
      IF(KMD.EQ.'END ') GO TO 500
      IF(KMD.EQ.'NORS') GO TO 130
      IF(KMD.EQ.'NORT') GO TO 130
      IF(KMD.EQ.'TAB ') GO TO 140
      IF(KMD.EQ.'LOG ') GO TO 150
      IF(KMD.EQ.'SNAP') GO TO 155
      IF(KMD.EQ.'ZERO') GO TO 160
      IF(KMD.EQ.'SEC ') GO TO 170
      IF(KMD.EQ.'LSEC') GO TO 170
C
      IF(KMD.EQ.'HUSH') GO TO 180
C
      IF(KMD.EQ.'RLIM') GO TO 200
      IF(KMD.EQ.'RLMM') GO TO 205
      IF(KMD.EQ.'GLIM') GO TO 205
C
      IF(KMD.EQ.'BPON') GO TO 210
      IF(KMD.EQ.'BPOF') GO TO 210
C
      IF(KMD.EQ.'NAMP') GO TO 220
C
      RETURN
C
  100 LISFLG=KMD
      GO TO 2000
C
  110 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)  !OPEN COMMAND FILE
      GO TO 2000
C
  125 CALL HELPMANU(IWD,LHEP,IHELP,300,20,IHEPF)
      GO TO 2000
C 
  130 CALL NORMAN
      GO TO 2000
C
  140 CALL TABO                         !LIST ARRAYS FOR DIAGNOSTICS
      GO TO 2000
C
  150 CALL READUM                       !READ SCALERS
      CALL LOGUM(LOGUT,LOGUP)           !LOG ON SCAD.LOG
      GO TO 2000
C
  155 CALL SNAP                         !WRITE SCALERS TO scadymdhms.snap
      GO TO 2000
C
  160 CALL ZOTUM                        !ZERO ALL SCALERS
      GO TO 2000
C
  170 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 1000
      IF(KMD.EQ.'LSEC') GO TO 172
      CALL LIMIV(LWD(1,2),1,20,IV,IERR)
      IF(IERR.NE.0) GO TO 1010
      SEC=IV
      GO TO 2000
C
  172 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1010
      IF(IV.EQ.0)   GO TO 174
      IF(IV.GE.300.AND.IV.LE.3600) GO TO 174
      GO TO 1020
  174 LSEC=IV
      GO TO 2000
C
  180 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 1000
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      CALL SECSENS70(TNOW)
      THUSH=TNOW+60*IV
      GO TO 2000
C
  200 CALL LIMSET
      GO TO 2000
C
  205 CALL GLIMSET
      GO TO 2000
C
  210 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      IF(IV.LT.0.OR.IV.GT.1000) GO TO 1060
C
      IF(KMD.EQ.'BPON') MXGOOD=IV
      IF(KMD.EQ.'BPOF') MXBEEP=IV
      GO TO 2000
C
  220 IF(IWDRAW(2).EQ.BLANK) THEN
      PRNAM=' '
      GO TO 2000
      ENDIF
      CALL FINAME(IWDRAW,5,80,PRNAM,IERR)
      IF(IERR.NE.0) GO TO 1000
      GO TO 2000
C
  500 CALL SCLR
      CALL EXIT
C
 1000 WRITE(6,1005)
 1005 FORMAT(1H ,'Syntax error or illegal value - Ignored')
      GO TO 1500
C
 1010 WRITE(6,1015)
 1015 FORMAT(1H ,'Illegal display interval - Legal range = 1,20')
      GO TO 1500
C
 1020 WRITE(6,1025)
 1025 FORMAT(1H ,'Illegal Log-interval - Legals = 0 or 300 to 3600')
      GO TO 1500
C
 1060 WRITE(6,1065)IV
 1065 FORMAT(1H ,'Illegal value =',I8,' - legal range = 0 to 1000')
      GO TO 1500
C
 1500 IERR=1
C
 2000 IDONE='YES '
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
C$PROG GNITTER   - Opens & processes graphic scaler init file
C
C     ******************************************************************
C     BY W.T. MILNER AT ORPH - LAST MODIFIED 05/22/2005
C     ******************************************************************
C
      SUBROUTINE GNITTER(IERR)
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
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD1/ GNITNAM
      CHARACTER*80 GNITNAM
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
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
      DATA         MXSCA,LU/8,17/
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
      GNITNAM=CNAM
C
C     ------------------------------------------------------------------
C     Initialize scaler labels  & limits for whatever reason
C     ------------------------------------------------------------------
C
      DO 20 J=1,MXSCA
      DO 10 I=1,3
      LAG(I,J)=BLANK
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
      IF(CWD(1).EQ.' ')    GO TO 50          !Ignore blanks
C
      IF(COMBYT.EQ.'#')    GO TO 50          !Ignore comments
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
      CALL LODUP(IWD,1,IA,LAG(1,N),1)        !load up the label
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
  530 WRITE(CMSSG,535)NSCA
  535 FORMAT(I3,' scalers set up')
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
      GNITNAM='Undefined!'
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG STATMAN   - Status display routine for SCAD
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
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD1/ GNITNAM
      CHARACTER*80 GNITNAM
C     ------------------------------------------------------------------
      COMMON/SCD2/ MSPRED,DISPSEC
      INTEGER*4    MSPRED,DISPSEC
C     ------------------------------------------------------------------
      COMMON/SD04/ SNITFIL(20)
      INTEGER*4    SNITFIL
C
      CHARACTER*80 SNITNAM
      EQUIVALENCE  (SNITNAM,SNITFIL)
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
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
C     Display tabular related status
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 FORMAT('===================================================',
     &       '=============')
C
  102 FORMAT('---------------------------------------------------',
     &       '-------------')
C
      WRITE(CMSSG,105)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
  105 FORMAT('TABULAR DISPLAY INFORMATION')
C
      WRITE(CMSSG,115)SNITNAM
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,120)SEC
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,125)LSEC
      CALL MESSLOG(LOGUT,LOGUP)
C
  115 FORMAT('Snit-file name   = ',A)
  120 FORMAT('Display interval = ',I5,' seconds')
  125 FORMAT('Log     interval = ',I5,' seconds')
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,130)
  130 FORMAT('NAME            C   N   A   F')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 150 N=1,NR
      WRITE(CMSSG,140)(LA(I,N),I=1,3),CN(N),SN(N),A(N),F(N)
  140 FORMAT(3A4,1X,4I4)
      CALL MESSLOG(LOGUT,LOGUP)
  150 CONTINUE
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Display tabular-limit related status
C     ------------------------------------------------------------------
C
      IF(NLIM.LE.0) GO TO 200
C
      WRITE(CMSSG,160)
  160 FORMAT('Tabular Limit Specifications    LOW        HIGH')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 170 J=1,NLIM
      N=SCNDX(J)
      WRITE(CMSSG,165)(LA(I,N),I=1,3),LIMLO(J),LIMHI(J)
  165 FORMAT(3A4,11X,2F12.1)
      CALL MESSLOG(LOGUT,LOGUP)
  170 CONTINUE
C
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Display graphic related status
C     ------------------------------------------------------------------
C
  200 IF(NSCA.LE.0) RETURN
C
      WRITE(CMSSG,210)
  210 FORMAT('GRAPHIC DISPLAY INFORMATION')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,220)GNITNAM
  220 FORMAT('Gnit-file name = ',A)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,230)DISPSEC
  230 FORMAT('Displays/sec   = ',I2)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,240)
  240 FORMAT('NAME            C   N   A   F  NAGV  STATE')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 300 N=1,NSCA
C
      STATE=' OFF'
C
      IF(RATTYP(N).EQ.'SCAL') STATE='  ON'
C
      WRITE(CMSSG,250)(LAG(I,N),I=1,3),CC(N),NN(N),AA(N),FF(N),
     &                RATAVG(N),STATE
  250 FORMAT(3A4,1X,4I4,I6,3X,A)
      CALL MESSLOG(LOGUT,LOGUP)
C
  300 CONTINUE
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Display Meter limit related status
C     ------------------------------------------------------------------
C
CX    IF(NLIMG.LE.0) RETURN
C
      WRITE(CMSSG,310)
  310 FORMAT('Meter Limit Specifications      LOW        HIGH')
C
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 320 J=1,NSCA
      WRITE(CMSSG,315)(LAG(I,J),I=1,3),GLIMLO(J),GLIMHI(J),GLIMON(J)
  315 FORMAT(3A4,11X,2F12.1,2X,A4)
      CALL MESSLOG(LOGUT,LOGUP)
  320 CONTINUE
C
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
      RETURN
C
      END
