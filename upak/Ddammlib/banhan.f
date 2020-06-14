C$PROG BANHAN    - Manages banana directives (zero, ins, add, etc)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BANHAN(IDW,MODE,XX,YY)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PL04/ ILOCX(20),IHICX(20),ILOCY(20),IHICY(20),  !/PL04
     &             KLOCX(20),KHICX(20),KLOCY(20),KHICY(20),  !/PL04
     &             KDDP(20),MINZZ,MAXZZ                      !/PL04
C     ------------------------------------------------------------------
      COMMON/PL09/ XL(64,21),YL(64,21),NXYL(21),IBL(21),
     &             IBWN(21),LNBAN(4,21),MAXXYL
C     ------------------------------------------------------------------
C
      CHARACTER*4  MODE
C
      DIMENSION XC(64),YC(64),XS(64),YS(64)
C   
c     INTEGER*4 BLACK,WHITE,RED,GREEN,BLUE,CYAN,MAGENTA,YELLOW
      character*4 BLACK,WHITE,RED,GREEN,BLUE,CYAN,MAGENTA,YELLOW
C   
      EQUIVALENCE (XC(1),XL(1,21)),
     &            (YC(1),YL(1,21)),
     &            (NXY,NXYL(21)),
     &            (IDOPN,IBL(21))
C   
      DATA BLACK,WHITE,RED,GREEN,BLUE,CYAN,MAGENTA,YELLOW/
     &    'ERAS','WHIT','RED ','GREE','BLUE','GRBL','RDBL','RDGR'/
C   
      DATA NXY/0/
      DATA MAXXYL,IBL/20,20*-1,0/

      integer*4 kol
      character*4 ckol
      equivalence (ckol,kol)
      DATA cKOL/'COM '/
C
      SAVE
C
C     ------------------------------------------------------------------

C   
      IF(MODE.EQ.'ZERO') GO TO 100
      IF(MODE.EQ.'ADD ') GO TO 200
      IF(MODE.EQ.'INS ') GO TO 250
      IF(MODE.EQ.'DEL ') GO TO 300
      IF(MODE.EQ.'REP ') GO TO 300
      IF(MODE.EQ.'ERAS') GO TO 400
      IF(MODE.EQ.'DRAW') GO TO 500
      IF(MODE.EQ.'LIST') GO TO 600
      RETURN
C   
  100 IF(NXY.GT.0) CALL BANDRA(IDW,IDOPN,KOL,XC,YC,NXY)
      NXY=0
      IDOPN=0
      RETURN
C   
  200 CALL BANDRA(IDW,IDOPN,KOL,XC,YC,NXY)
      NXY=NXY+1
      XC(NXY)=XX
      YC(NXY)=YY
      CALL BANDRA(IDW,IDOPN,KOL,XC,YC,NXY)
      RETURN
C   
  250 DIST=1.0E8
      NDX=0
      NDO=NXY-1
      IF(NDO.LT.1) RETURN
C
      DO 260 I=1,NDO
      XA=XC(I)
      YA=YC(I)
      IF(IMOUT(IDW,XA,YA).NE.0) GO TO 260
      XB=XC(I+1)
      YB=YC(I+1)
      IF(IMOUT(IDW,XB,YB).NE.0) GO TO 260
C   
      CALL PLDIST(XA,YA,XB,YB,XX,YY,R,IERR)
C   
      IF(IERR.NE.0) GO TO 260
C   
      IF(R.LT.DIST) THEN
                    NDX=I+1
                    DIST=R
                    ENDIF
C   
  260 CONTINUE
      GO TO 315
C   
  300 DIST=1.0E8
      NDX=0
C   
      DO 310 I=1,NXY
      IF(IMOUT(IDW,XC(I),YC(I)).NE.0) GO TO 310
      DX=ABS(XX-XC(I))
      DY=ABS(YY-YC(I))
      R=SQRT(DX*DX+DY*DY)
      IF(R.LT.DIST) THEN
                    NDX=I
                    DIST=R
                    ENDIF
  310 CONTINUE
C   
  315 IF(NDX.EQ.0) RETURN
C   
      CALL BANDRA(IDW,IDOPN,KOL,XC,YC,NXY)
C   
      IF(MODE.EQ.'INS ') GO TO 350
C   
      IF(MODE.EQ.'REP ') THEN
                         XC(NDX)=XX
                         YC(NDX)=YY
                         GO TO 330
                         ENDIF
C   
      N=0
      DO 320 I=1,NXY
      IF(I.EQ.NDX) GO TO 320
      N=N+1
      XC(N)=XC(I)
      YC(N)=YC(I)
  320 CONTINUE
C   
      NXY=N
      IF(NXY.LE.0) RETURN
C   
  330 CALL BANDRA(IDW,IDOPN,KOL,XC,YC,NXY)
      RETURN
C   
  350 N=0
      DO 360 I=1,NXY
      IF(I.NE.NDX) GO TO 355
      N=N+1
      XS(N)=XX
      YS(N)=YY
  355 N=N+1
      XS(N)=XC(I)
      YS(N)=YC(I)
  360 CONTINUE
C   
      NXY=NXY+1
      DO 365 I=1,NXY
      XC(I)=XS(I)
      YC(I)=YS(I)
  365 CONTINUE
C   
      CALL BANDRA(IDW,IDOPN,KOL,XC,YC,NXY)
      RETURN
C   
  400 CALL BANDRA(IDW,IDOPN,KOL,XC,YC,NXY)
      RETURN
C   
  500 CALL BANDRA(IDW,IDOPN,KOL,XC,YC,NXY)
      RETURN
C   
  600 DO 610 I=1,NXY
      WRITE(LOGUT,605)XC(I),YC(I)
  605 FORMAT(1H ,2F6.0)
  610 CONTINUE
      WRITE(LOGUT,615)ILOCX(IDW),IHICX(IDW)
      WRITE(LOGUT,615)ILOCY(IDW),IHICY(IDW)
  615 FORMAT(1H ,2I6)
      RETURN
      END
