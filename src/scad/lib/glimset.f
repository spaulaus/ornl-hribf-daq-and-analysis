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
