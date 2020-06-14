C$PROG PLOTFIT   - Displays fit results for program FITU
C
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/17/2000
C     ******************************************************************
C
      SUBROUTINE PLOTFIT(KIND,XIN,YIN,YCAL,NP,XLIN,YLIN,NLIN)
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
      COMMON/FT12/ TITLA,TITLB,TITLC
      CHARACTER*80 TITLA,TITLB,TITLC
C     ------------------------------------------------------------------
      COMMON/FT13/ XMIN,XMAX,YMIN,YMAX
      REAL*4       XMIN,XMAX,YMIN,YMAX
C     ------------------------------------------------------------------
      COMMON/FT14/ IDW,KDSP
      INTEGER*4    IDW,KDSP
C     ------------------------------------------------------------------
      COMMON/FT16/ XONOF
      CHARACTER*4  XONOF
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                                    ISOPEN
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL,    IBANNER
C     ------------------------------------------------------------------
      CHARACTER*80 CTITL
      EQUIVALENCE (CTITL,ITITL)
C     ------------------------------------------------------------------
      CHARACTER*4  KMD,KMX,KMY,KOLR
C
      REAL*8       XIN(*),YIN(*),YCAL(*)
C
      REAL*4       XLIN(*),YLIN(*)
C
      INTEGER*4    KIND,NP,NLIN
C
      INTEGER*4    JWD(20),ISTAT,LU,IERR,I
C
      REAL*4       X(512),Y(512),C(512),U(512)
C
      REAL*4       XX(2),YY(2),XLO,XHI,YLO,YHI
C
      EQUIVALENCE (KMX,JWD(1)),(KMY,JWD(2))
C
      character*4  cjwd(20)
      equivalence  (cjwd, jwd)
      DATA        cJWD/20*'    '/
C     ------------------------------------------------------------------
C
      IF(XONOF.NE.'XON ') RETURN
C
      IF(ISOPEN.NE.'YES ') THEN
      KMX='FIG '
      KMY='  11'
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
      CALL ERRCOMP(U)
C
      DO 20 I=1,NP
C
      X(I)=XIN(I)
      Y(I)=YIN(I)
      C(I)=YCAL(I)
C
      IF(X(I).LT.XLO) XLO=X(I)
      IF(X(I).GT.XHI) XHI=X(I)
C
      IF(Y(I)-U(I).LT.YLO) YLO=Y(I)-U(I)
      IF(Y(I)+U(I).GT.YHI) YHI=Y(I)+U(I)
C
      IF(C(I).LT.YLO) YLO=C(I)
      IF(C(I).GT.YHI) YHI=C(I)
C
   20 CONTINUE
C
      IF(XLIN(1)   .LT.XLO) XLO=XLIN(1)
      IF(XLIN(NLIN).GT.XHI) XHI=XLIN(NLIN)
C
      DO 30 I=1,NLIN
      IF(YLIN(I).LT.YLO) YLO=YLIN(I)
      IF(YLIN(I).GT.YHI) YHI=YLIN(I)
   30 CONTINUE
C
      XLO=XLO-0.05*(XHI-XLO)
      XHI=XHI+0.05*(XHI-XLO)
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
      WINFLG(1,IDW)=0
      WINFLG(4,IDW)=KDSP
      WINDAT(5,IDW)=XLO
      WINDAT(6,IDW)=YLO
      WINDAT(7,IDW)=XHI
      WINDAT(8,IDW)=YHI
      IF(KIND.EQ.1) CTITL=TITLA
      IF(KIND.EQ.2) CTITL=TITLB
      IF(KIND.EQ.3) CTITL=TITLC
      CALL XX_WINMAN('WIN ',IDW)
      CALL XX_SYNC(DPY,.TRUE.)
C
      CALL PLOTXYSYM(IDW,KOLR,X,Y,NP)
C
      CALL PLOTXYLIN(IDW,KOLR,XLIN,YLIN,NLIN)
C
      DO 50 I=1,NP
      XX(1)=X(I)
      XX(2)=X(I)
      YY(1)=Y(I)-U(I)
      YY(2)=Y(I)+U(I)
      CALL PLOTXYLIN(IDW,KOLR,XX,YY,2)
   50 CONTINUE
C
      RETURN
C
      END
