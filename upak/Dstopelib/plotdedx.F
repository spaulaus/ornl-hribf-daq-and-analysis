C$PROG PLOTDEDX  - Displays fit results for program STOPIT
C
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE PLOTDEDX(X,YA,YB,YC,NP)
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
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ST01/ KSORBSAV,KINTARG,LEM,NLST,ITPF,AP,ZP,AT,ZT,EA,EB,DE
      CHARACTER*4  KSORBSAV,KINTARG,LEM
      INTEGER*4                         NLST,ITPF
      REAL*4                                      AP,ZP,AT,ZT,EA,EB,DE
C     ------------------------------------------------------------------
      COMMON/ST02/ KSORB
      CHARACTER*4  KSORB
C     ------------------------------------------------------------------
      COMMON/ST04/ XMIN,XMAX,YMIN,YMAX
      REAL*4       XMIN,XMAX,YMIN,YMAX
C     ------------------------------------------------------------------
      COMMON/ST05/ IDW,KDSP
      INTEGER*4    IDW
      CHARACTER*4      KDSP
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if   (PTRSIZE ==64)
      INTEGER*8    DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
#endif
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL
      CHARACTER*4            IBANNER
C     ------------------------------------------------------------------
      CHARACTER*80 CTITL
      EQUIVALENCE (CTITL,ITITL)
C     ------------------------------------------------------------------
      REAL*4       X(*),YA(*),YB(*),YC(*)
C
      INTEGER*4    NP
C
      INTEGER*4    NUCNAME,NAMTARG,NAMPROJ,IAP,IAT,IZP,IZT
C
      INTEGER*4    JWD(20),ISTAT,LU,IERR,I
C
      CHARACTER*4  CJWD(20),KMD,KOLR
C
      EQUIVALENCE (CJWD,JWD)
C
      REAL*4       XLO,XHI,YLO,YHI
C
      DATA         cJWD/20*'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(ISOPEN.NE.'YES ') THEN
      CJWD(1)='FIG '
      CJWD(2)='  11'
      LU=1
      CALL NEWFIG(LU,JWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL LABLMAN('INIT',0,0,0,0)
      ENDIF
C
      XLO=1.0E10
      XHI=0.0
      YLO=1.0E10
      YHI=0.0
C
      DO 20 I=1,NP
C
      IF(XMIN.GT.0.0.AND.X(I).LT.XMIN) GO TO 20
      IF(XMAX.GT.0.0.AND.X(I).GT.XMAX) GO TO 20
C
      IF(X(I).LT.XLO) XLO=X(I)
      IF(X(I).GT.XHI) XHI=X(I)
C
      IF(YA(I).LE.0.0) GO TO 10
C
      IF(YA(I).LT.YLO) YLO=YA(I)
      IF(YA(I).GT.YHI) YHI=YA(I)
C
   10 IF(YB(I).LE.0.0) GO TO 12
C
      IF(YB(I).LT.YLO) YLO=YB(I)
      IF(YB(I).GT.YHI) YHI=YB(I)
C
   12 IF(YC(I).LE.0.0) GO TO 20
C
      IF(YC(I).LT.YLO) YLO=YC(I)
      IF(YC(I).GT.YHI) YHI=YC(I)
C
   20 CONTINUE
C
      XLO=XLO-0.05*(XHI-XLO)
      XHI=XHI+0.05*(XHI-XLO)
      YHI=YHI+0.05*(YHI-YLO)
      IF(YLO.LT.1.0) YLO=1.0
C
      IF(XMIN.NE.0.0.OR.XMAX.NE.0.0) THEN
      XLO=XMIN
      XHI=XMAX
      ENDIF
C
      IF(YMIN.NE.0.0.OR.YMAX.NE.0.0) THEN
      YLO=YMIN
      YHI=YMAX
      ENDIF
C
      KOLR='WHIT'
      CALL WAIT(200,1,ISTAT)
C
CX    CALL XX_WINMAN('ERAS',IDW)
C
      WINFLG(1,IDW)=0
      WINFLC(4,IDW)=KDSP
      WINDAT(5,IDW)=XLO
      WINDAT(6,IDW)=YLO
      WINDAT(7,IDW)=XHI
      WINDAT(8,IDW)=YHI
C
      IAP=AP+0.5
      IAT=AT+0.5
      IZP=ZP+0.5
      IZT=ZT+0.5
C
      NAMTARG=NUCNAME(IZT)
      NAMPROJ=NUCNAME(IZP)
C
      IF(KINTARG.EQ.'    ') WRITE(CTITL,25)IAP,NAMPROJ,IAT,NAMTARG
C
      IF(KINTARG.NE.'    ') WRITE(CTITL,30)IAP,NAMPROJ,KINTARG
C
   25 FORMAT('100*(Stopping pwr (MeV*cmsq/mg)) of ',I3,A4,'in  ',I3,A4,
     &       '  vs  MeV')
C
   30 FORMAT('100*(Stopping pwr (MeV*cmsq/mg)) of ',I3,A4,'in  ',A4,
     &       '  vs  MeV')
C
      CALL XX_WINMAN('WIN ',IDW)
      CALL XX_SYNC(DPY,.TRUE.)
C
      CALL PLOTXYSYM(IDW,KOLR,X,YA,NP)
C
      CALL PLOTXYLIN(IDW,KOLR,X,YB,NP)
C
      CALL PLOTXYFIL(IDW,KOLR,X,YC,NP)
C
      RETURN
C
      END
