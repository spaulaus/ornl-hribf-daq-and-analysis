C$PROG NUMETER   - Creates a new meter window & draws scale, etc
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/19/2002
C     ******************************************************************
C
      SUBROUTINE NUMETER(N,CMUL,ERRFLG)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                             !STAR-8 on Alpha
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
      INTEGER*4    GCOR,   GCON                         !STAR-8 on Alpha
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL,    IBANNER
      CHARACTER*80 CTITL
      EQUIVALENCE (CTITL,ITITL)
C     ------------------------------------------------------------------
      COMMON/ME00/SQSIZ(20),FACRAD(20),NUDSP(20),NUDEC(20)
      REAL*4      SQSIZ,    FACRAD
      INTEGER*4                        NUDSP,    NUDEC
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C     ------------------------------------------------------------------
      REAL*4       CNTLN(13),CNTLG(12),CNTDEC(10)
C
      REAL*4       ANG(50),X1(50),X2(50),Y1(50),Y2(50)
C
      REAL*4       TICLN(25),TICLG(21)
C
      INTEGER*4    IX1(50),IX2(50),IY1(50),IY2(50)
C
      INTEGER*4    N,I,IMUL,K,NDO
C
      REAL*4       W,H
C
      CHARACTER*1  LALN1(10),LALG1(9),LADEC(10),CNUM1
C
      CHARACTER*2  LALN2(3), LALG2(3),CNUM2
C
      DATA LADEC/'0','1', '2', '3', '4', '5', '6', '7', '8', '9'/
C
      DATA LALN1/'0','1', '2', '3', '4', '5', '6', '7', '8', '9'/
      DATA LALN2/'10','11','12'/
C
      DATA LALG1/'1', '2', '3', '4', '5', '6', '7', '8', '9'/
      DATA LALG2/'10','15','20'/
C
      DATA CNTLN/0,1,2,3,4,5,6,7,8,9,10,11,12/
C
      DATA CNTLG/1,2,3,4,5,6,7,8,9,10,15,20/
C
      DATA CNTDEC/0,1,2,3,4,5,6,7,8,9/
C
      DATA TICLN/0.0, 0.5,
     &           1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,
     &           5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5,
     &           10.0, 10.5, 11.0, 11.5, 12.0/
C
      DATA TICLG/1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,
     &     5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5,
     &     10., 15., 20./
C
      REAL*4       DTOR,ALN,BLN,ALG,BLG,ADC,BDC
C
      DATA         DTOR/0.017453292/
C
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
C                                               !for CNT=1,  ANG=225 deg
C                                               !for CNT=9,  ANG=-45 deg
C     ------------------------------------------------------------------
C
C
      INTEGER*2    PXY(2,3)
C
      INTEGER*4    FACSIZ(20)
C
      CHARACTER*12 CLA(16),CHMUL
C
      EQUIVALENCE (CLA,LAG(1,1))
C
      REAL*8       CMUL
C
      CHARACTER*8  ERRFLG
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
C     Draw meter face
C     ------------------------------------------------------------------
C
      W=WINDAT(3,N)                       !Window width (pixels)
      H=WINDAT(4,N)                       !Window height(pixels)
      SQSIZ(N)=W                          !Size of contained square
      IF(H.LT.SQSIZ(N)) SQSIZ(N)=H        !Size of contained square
      FACSIZ(N)=SQSIZ(N)-50               !Face diameter (pixels)
      FACRAD(N)=0.5*FACSIZ(N)             !Face radius   (pixels)
C
      IF(ERRFLG.NE.' ') THEN
      CHMUL=ERRFLG
      GO TO 100
      ENDIF
C
      IMUL=1.0/CMUL+0.5
C
      CHMUL='******'
C
      IF(IMUL.EQ.1)         CHMUL='  x1'
      IF(IMUL.EQ.10)        CHMUL='  x10'
      IF(IMUL.EQ.100)       CHMUL='  x10**2'
      IF(IMUL.EQ.1000)      CHMUL='  x10**3'
      IF(IMUL.EQ.10000)     CHMUL='  x10**4'
      IF(IMUL.EQ.100000)    CHMUL='  x10**5'
      IF(IMUL.EQ.1000000)   CHMUL='  x10**6'
      IF(IMUL.EQ.10000000)  CHMUL='  x10**7'
      IF(IMUL.EQ.100000000) CHMUL='  x10**8'
      IF(IMUL.GT.100000000) CHMUL='  ov-rng'
C
C
  100 WRITE(CTITL,105)CLA(N),CHMUL
  105 FORMAT(A,A)
C
      WINFLG(1,N)=0
      WINFLC(2,N)='OFF '
C
      CALL XX_WINMAN('WIN ',N)
C
      CALL FILLCIR(N,GCOR(2),25,25,FACSIZ(N),FACSIZ(N))
C
      NDO=12
C
      IF(DISPTYP.EQ.'GLIN') NDO=13
C
      DO 120 I=1,NDO
C
      IF(DISPTYP.EQ.'GLIN') ANG(I)=ALN+BLN*CNTLN(I)
C
      IF(DISPTYP.EQ.'GLOG') ANG(I)=ALG+BLG*ALOG(CNTLG(I))
C
      X1(I)= FACRAD(N)*COS(DTOR*ANG(I))
      Y1(I)= FACRAD(N)*SIN(DTOR*ANG(I))
      X2(I)=(FACRAD(N)+8)*COS(DTOR*ANG(I))
      Y2(I)=(FACRAD(N)+8)*SIN(DTOR*ANG(I))
C
      X1(I)=0.5*SQSIZ(N)+X1(I)
      X2(I)=0.5*SQSIZ(N)+X2(I)
      Y1(I)=0.5*SQSIZ(N)-Y1(I)
      Y2(I)=0.5*SQSIZ(N)-Y2(I)
C
      IX1(I)=X1(I)+0.5
      IX2(I)=X2(I)+0.5
      IY1(I)=Y1(I)+0.5
      IY2(I)=Y2(I)+0.5
C
  120 CONTINUE
C
C     ------------------------------------------------------------------
C     Draw tic labels
C     ------------------------------------------------------------------
C
      K=0
C
      NDO=9
C
      IF(DISPTYP.EQ.'GLIN') NDO=10
C
      DO 310 I=1,NDO
      K=K+1
      CNUM1=LALN1(I)
      IF(DISPTYP.EQ.'GLOG') CNUM1=LALG1(I)
      CALL XX_DRAWSTRING(DPY,WDID(N),GCON(2),IX2(K),IY2(K),CNUM1)
  310 CONTINUE
C
      DO 320 I=1,3
      K=K+1
      CNUM2=LALN2(I)
      IF(DISPTYP.EQ.'GLOG') CNUM2=LALG2(I)
      CALL XX_DRAWSTRING(DPY,WDID(N),GCON(2),IX2(K),IY2(K),CNUM2)
  320 CONTINUE
C
      CALL XX_SYNC(DPY,.TRUE.)
C
C     ------------------------------------------------------------------
C     Locate & draw tic-marks
C     ------------------------------------------------------------------
C
      NDO=21
C
      IF(DISPTYP.EQ.'GLIN') NDO=25
C
      DO 350 I=1,NDO
C
      IF(DISPTYP.EQ.'GLIN') ANG(I)=ALN+BLN*TICLN(I)
C
      IF(DISPTYP.EQ.'GLOG') ANG(I)=ALG+BLG*ALOG(TICLG(I))
C
      X1(I)= FACRAD(N)*COS(DTOR*ANG(I))
      Y1(I)= FACRAD(N)*SIN(DTOR*ANG(I))
      X2(I)=(FACRAD(N)+8)*COS(DTOR*ANG(I))
      Y2(I)=(FACRAD(N)+8)*SIN(DTOR*ANG(I))
C
      X1(I)=0.5*SQSIZ(N)+X1(I)
      X2(I)=0.5*SQSIZ(N)+X2(I)
      Y1(I)=0.5*SQSIZ(N)-Y1(I)
      Y2(I)=0.5*SQSIZ(N)-Y2(I)
C
  350 CONTINUE
C
      DO 360 I=1,NDO
C
      IX1(I)=X1(I)+0.5
      IX2(I)=X2(I)+0.5
      IY1(I)=Y1(I)+0.5
      IY2(I)=Y2(I)+0.5
C
      CALL XX_DRAWLINE(DPY,WDID(N),GCON(2),IX1(I),IY1(I),IX2(I),IY2(I))
C
  360 CONTINUE
C
      CALL XX_SYNC(DPY,.TRUE.)
C
C     ------------------------------------------------------------------
C     Locate & draw decade labels
C     ------------------------------------------------------------------
C
      NDO=10
C
      DO 370 I=1,NDO
C
      ANG(I)=ADC+BDC*CNTDEC(I)
C
      X1(I)= 0.4*FACRAD(N)*COS(DTOR*ANG(I))
      Y1(I)= 0.4*FACRAD(N)*SIN(DTOR*ANG(I))
      X2(I)=(0.4*FACRAD(N)+8)*COS(DTOR*ANG(I))
      Y2(I)=(0.4*FACRAD(N)+8)*SIN(DTOR*ANG(I))
C
      X1(I)=0.5*SQSIZ(N)+X1(I)
      X2(I)=0.5*SQSIZ(N)+X2(I)
      Y1(I)=0.5*SQSIZ(N)-Y1(I)
      Y2(I)=0.5*SQSIZ(N)-Y2(I)
C
  370 CONTINUE
C
      DO 380 I=1,NDO
C
      IX1(I)=X1(I)+0.5
      IX2(I)=X2(I)+0.5
      IY1(I)=Y1(I)+0.5
      IY2(I)=Y2(I)+0.5
C
      CNUM1=LADEC(I)
C
      CALL XX_DRAWSTRING(DPY,WDID(N),GCOR(2),IX2(I),IY2(I),CNUM1)
C
  380 CONTINUE
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NUDSP(N)=0
C
      NUDEC(N)=0
C
      RETURN
C
      END
