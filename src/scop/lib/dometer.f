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
