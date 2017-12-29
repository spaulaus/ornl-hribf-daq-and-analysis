C$PROG LIMSET    - Sets up rate-limits for display & test
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LIMSET
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
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
      DATA         NLIM,MXLIM/0,10/
C     ------------------------------------------------------------------
      INTEGER*4    LWD3(3,40),ITYP3(40),NF3,NTER3
C
      INTEGER*4    LABNDX,NDX,IV,KIND,IERR,IT,N,I,J,IDX
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
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMX.EQ.'SHO ') GO TO 100
      IF(KMX.EQ.'OFF ') GO TO 200
C
C
      CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,SETW,12,NTER3)
C
      CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,1,80,NTER3)
C
      IF(NTER3.NE.0)    GO TO 500
C
      NDX=LABNDX(LWD3(1,2))
C
      IF(NDX.LE.0)     GO TO 510
C
C
      IF(KMY.EQ.'OFF ') THEN
      CALL LIMSAV(NDX,'DEL ',XLO,XHI)
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
C
      CALL LIMSAV(NDX,'SAV ',XLO,XHI)
      GO TO 1000
C
C     ------------------------------------------------------------------
C     Display current limit table
C     ------------------------------------------------------------------
C
  100 DO 120 N=1,NLIM
      IDX=SCNDX(N)
      WRITE(6,105)(LA(I,IDX),I=1,3),LIMLO(N),LIMHI(N)
  105 FORMAT(1H ,3A4,2F10.3)
  120 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Turn OFF all limits
C     ------------------------------------------------------------------
C
  200 NLIM=0
      DO 210 I=1,MXLIM
      SCNDX(I)=0
      LIMLO(I)=0.0
      LIMHI(I)=0.0
  210 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Report error messages
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error in rate limit definition - cmd ignored')
      GO TO 900
C
  510 WRITE(CMSSG,515)(LWD3(I,2),I=1,3)
  515 FORMAT('Undefined scaler label = ',3A4,' - cmd ignored')
      GO TO 900
C
  520 WRITE(CMSSG,525)
  525 FORMAT('Syntax error in rate lo-limit definition - cmd ignored')
      GO TO 900
C
  530 WRITE(CMSSG,535)
  535 FORMAT('Syntax error in rate hi-limit definition - cmd ignored')
      GO TO 900
C
  900 CALL MESSLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,SETW,8,NTER3)
      RETURN
      END
