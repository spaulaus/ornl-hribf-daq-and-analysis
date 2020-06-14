C$PROG BANSUM    - Sums counts in banana & does X & Y projections
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BANSUM(IDW,IBN,X,Y,NXY,ISAV)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO                                       !/DML3
      INTEGER*4    IDL,NCHL,KOLR,NNID                        !/DML3
      CHARACTER*4             KFL,                  KDST     !/DML3
C     ------------------------------------------------------------------
      COMMON/PL03/ IDAT(4096),MINCN(2,20),MAXCN(2,20),MINZ,MAXZ
C     ------------------------------------------------------------------
C
      CHARACTER*4  ISAV
C   
      DIMENSION    ADAT(4096),X(64),Y(64),KX(64),KY(64)
C
      REAL*8       SUMI
C
      INTEGER*4    ANUM(3)
C   
      EQUIVALENCE (ADAT,IDAT)
      character*4 ckindx, ckindy
      equivalence (ckindx,kindx), (ckindy,kindy)
C   
      DATA LXG,LYG,DEG0,DEG90,cKINDX,cKINDY/0,0,0.0,90.0,'PX  ','PY  '/
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IDN=IDL(1,IDW)
C
      CALL LUGET(KFL(1,IDW),LUH,LUD,KINF,IERR)
C
      DO 10 I=1,NXY
      KX(I)=X(I)+0.5
      KY(I)=Y(I)+0.5
   10 CONTINUE
C   
      CALL PROJ(LUD,LUH,IDN,IDAT,ADAT,KX,KY,NXY,LXG,LYG,DEG0,NCC,
     &          IERR)
C   
      IF(IERR.NE.0) RETURN
C   
      SUMI=0.0D0
      DO 20 I=1,NCC
      IDAT(I)=ADAT(I)+0.5
      SUMI=SUMI+IDAT(I)
   20 CONTINUE
C   
      WRITE(CMSSG,110)
  110 FORMAT('BID          SUM')
      CALL MESSLOG(LOGUT,LOGUP)
      CALL DFASCII(SUMI,ANUM,11)
      WRITE(CMSSG,115)IBN,ANUM
  115 FORMAT(I4,1X,2A4,A3)
      CALL MESSLOG(LOGUT,LOGUP)
C   
      IF(ISAV.NE.'YES ') RETURN
C   
      CALL PROSAV(KINDX,NCC,IERR)
      IF(IERR.NE.0) RETURN
C   
      CALL PROJ(LUD,LUH,IDN,IDAT,ADAT,KX,KY,NXY,LXG,LYG,DEG90,NCC,
     &          IERR)
C   
      IF(IERR.NE.0) RETURN
C   
      DO 120 I=1,NCC
      IDAT(I)=ADAT(I)+0.5
  120 CONTINUE
C   
      CALL PROSAV(KINDY,NCC,IERR)
C   
      RETURN
      END
