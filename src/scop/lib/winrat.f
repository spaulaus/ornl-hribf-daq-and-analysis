C$PROG WINRAT    - Sets up window for RATE display
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/04/2000
C     ******************************************************************
C
      SUBROUTINE WINRAT
C
      IMPLICIT NONE
C
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
      COMMON/DD15/ NCALLRI
      INTEGER*4    NCALLRI
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                                     !STAR-8
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/XLBB/ AA(2,20),BB(2,20),PLOTYP(20)
      REAL*4       AA,      BB
      CHARACTER*4                    PLOTYP
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
      INTEGER*4    GCOR,   GCON                                  !STAR-8
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL,    IBANNER
C     ==================================================================
      CHARACTER*1  CNUMA(10),CNUMB(18)
      DATA         CNUMA/'0','1','2','3','4','5','6','7','8','9'/
      DATA         CNUMB/'1','2','3','4','5','6','7','8','9',
     &                   '1','2','3','4','5','6','7','8','9'/
C     ------------------------------------------------------------------
      CHARACTER*80 CTITL
      EQUIVALENCE (CTITL,ITITL)
C
      REAL*4       X1,X2,Y1,Y2,WPIX,HPIX
C
      REAL*4       DELPX,DELPY,DELXV,DELYV,DELPX2
C
      INTEGER*4    ID,MODE,PXOF,PYOF,PXMX,PYMX,PXMM,PYMM,NPX,NPY,NJ,NK,I
C
      INTEGER*4    JFIR,JINC,KFIR,KINC,MUL,HIPIX,JNOW,KKK
C
      INTEGER*4    PXA,PXB,PYA,PYB
C
      SAVE
C
C     ------------------------------------------------------------------
C     ID    = WINDOW ORDINAL NUMBER
C     X1    = X-COOR OF LL-CORNER (USER UNITS)
C     Y1    = Y-COOR OF LL-CORNER (USER UNITS)
C     X2    = X-COOR OF UR-CORNER (USER UNITS)
C     Y2    = Y-COOR OF UR-CORNER (USER UNITS)
C     WPIX  = WIDTH  (PIXELS)
C     HPIX  = HEIGHT (PIXELS)
C     ------------------------------------------------------------------
C
      ID=1
C
      IF(ID.NE.0) RETURN
C
CX    CALL XX_WINMAN('ERAS',ID)
C
      WINDAT(1,ID)=0
      WINFLC(2,ID)='OFF '
      CTITL=' '
C
      WRITE(CTITL,10)(RATTYP(I),RATPAR(I),I=1,3)
   10 FORMAT('SCOPMO Rate - ',A4,I3,' - ',A4,I3,' - ',A4,I3)
C
      CALL XX_WINMAN('WIN ',ID)
C
      X1=0.0
      X2=1000.0
      Y1=0.0
      Y2=1500.0
      WPIX=WINDAT(3,ID)
      HPIX=WINDAT(4,ID)
C
      WINFLC(4,ID)=RATDSP
C
      PXOF=0
      PYOF=0
C
      DELPX=WPIX-PXOF         !# OF X-PIXELS AVAILABLE FOR PLOT
      DELPY=HPIX-PYOF         !# OF Y-PIXELS AVAILABLE FOR PLOT
C
      PXMX=WPIX
      PYMX=DELPY-20
C
      PYMM=PYMX/2
      PXMM=PXMX/2
C
      PLOTYP(ID)=RATDSP       !PLOT-TYPE ('LIN ' OR 'LOG ')
C
      DELXV=X2-X1+1.0         !X-RANGE
      DELYV=Y2-Y1+1.0         !Y-RANGE
C
      IF(RATDSP.EQ.'LOG ') GO TO 100
C
      BB(1,ID)=DELPX/DELXV
      AA(1,ID)=PXOF-BB(1,ID)*X1
C
      BB(2,ID)=-DELPY/DELYV
      AA(2,ID)=DELPY-BB(2,ID)*Y1
C
C     ------------------------------------------------------------------
C     Do linear scale
C     ------------------------------------------------------------------
C
      DELPX2=2.0*DELPX
C
      CALL TICKL(X1,X2,DELPX2,JFIR,JINC,NJ,KFIR,KINC,NK)
C
      DO 30 I=1,NK
      NPX=AA(1,ID)+BB(1,ID)*KFIR+0.5
      CALL XX_DRAWLINE(DPY,WDID(ID),GCON(2),NPX,PYMX,NPX,PYMX-10)
      KFIR=KFIR+KINC
   30 CONTINUE
C
      KKK=0
      DO 40 I=1,NJ
      NPX=AA(1,ID)+BB(1,ID)*JFIR+0.5
      CALL XX_DRAWLINE(DPY,WDID(ID),GCON(2),NPX,PYMX,NPX,0)
      JFIR=JFIR+JINC
      KKK=KKK+1
C
      IF(KKK.GE.2.AND.KKK.LE.10) THEN
      CALL XX_DRAWSTRING(DPY,WDID(ID),GCON(2),NPX,PYMX+12,CNUMA(KKK))
      ENDIF
      IF(KKK.EQ.4.OR.KKK.EQ.6.OR.KKK.EQ.8) THEN
      CALL XX_DRAWLINE(DPY,WDID(ID),GCON(2),NPX-1,PYMX,NPX-1,0)
      CALL XX_DRAWLINE(DPY,WDID(ID),GCON(2),NPX+1,PYMX,NPX+1,0)
      ENDIF
   40 CONTINUE
C
      CALL XX_DRAWLINE(DPY,WDID(ID),GCON(2),0,PYMX,PXMX,PYMX)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NCALLR=0
C
      NCALLRI=0
C
      RETURN
C
C     ------------------------------------------------------------------
C     Do log scale
C     ------------------------------------------------------------------
C
  100 X1=1.0
      X2=100.0
      KKK=0
C
      BB(1,ID)=DELPX/(ALOG(X2)-ALOG(X1))
      AA(1,ID)=DELPX-BB(1,ID)*ALOG(X2)
C
      BB(2,ID)=-DELPY/DELYV
      AA(2,ID)=DELPY-BB(2,ID)*Y1
C
      CALL LOGTIC(X1,X2,JFIR,MUL)
C
      JNOW=MUL*JFIR
  110 IF(JNOW.GT.X2) GO TO 200
C
      NPX=AA(1,ID)+BB(1,ID)*ALOG(FLOAT(JNOW))
      HIPIX=PYMX
C
      CALL XX_DRAWLINE(DPY,WDID(ID),GCON(2),NPX,PYMX,NPX,PYMX-HIPIX)
C
      KKK=KKK+1
C
      CALL XX_DRAWSTRING(DPY,WDID(ID),GCON(2),NPX,PYMX+12,CNUMB(KKK))
C
      IF(MUL.EQ.5.OR.MUL.EQ.1) THEN
      CALL XX_DRAWLINE(DPY,WDID(ID),GCON(2),NPX-1,PYMX,NPX-1,0)
      CALL XX_DRAWLINE(DPY,WDID(ID),GCON(2),NPX+1,PYMX,NPX+1,0)
      ENDIF
C
C
  120 MUL=MUL+1
      JNOW=MUL*JFIR
      IF(MUL.EQ.10) THEN
                    JFIR=JNOW
                    MUL=1
                    ENDIF
      GO TO 110
C
  200 CALL XX_DRAWLINE(DPY,WDID(ID),GCON(2),0,PYMX,PXMX,PYMX)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NCALLR=0
C
      NCALLRI=0
C
      RETURN
C
      END
