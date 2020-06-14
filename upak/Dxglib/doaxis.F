C$PROG DOAXIS    - Draws window axis, tick-marks & labels
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE DOAXIS(ID,MODE,X1,Y1,X2,Y2,WPIX,HPIX)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if (PTRSIZE == 64)
      INTEGER*8    DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
#endif
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
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
#if (PTRSIZE == 64)
      INTEGER*8    GCOR,   GCON                             !Alpha STAR8
#else
      INTEGER*4    GCOR,   GCON                             !Alpha STAR8
#endif
C     ------------------------------------------------------------------
#if (PTRSIZE == 64)
      INTEGER*8   LGC                                       !Alpha STAR8
#else
      INTEGER*4   LGC                                       !Alpha STAR8
#endif
C
      INTEGER*4   ANUM(3)     !ASCII-NUMBER BUFFER
      CHARACTER*6 CNUM        !ASCII-NUMBER BUFFER
      EQUIVALENCE (CNUM,ANUM) !ASCII-NUMBER BUFFER
C
      INTEGER*4   ID          !WINDOW  ID
      CHARACTER*4 MODE        !Display mode LIN/LOG
      REAL*4 X1               !MINIMUM X-VALUE
      REAL*4 X2               !MAXIMUM X-VALUE
      REAL*4 Y1               !MINIMUM Y-VALUE
      REAL*4 Y2               !MAXIMUM Y-VALUE
C     REAL*4 AA               !CONVERSION CONSTANT
C     REAL*4 BB               !CONVERSION CONSTANT
C     REAL*4 WINDAT           !WINDOW DATA ARRAY
      REAL*4 WPIX             !WINDOW WIDTH  (PIXELS)
      REAL*4 HPIX             !WINDOW HEIGHT (PIXELS)
C
      REAL*4    DELPX,DELPY,DELPZ,DELXV,DELYV
C
      INTEGER*4 PXOF,PYOF,PXMX,PYMX
C
      INTEGER*4 JFIR,JINC,KFIR,KINC,NJ,NK
C
      INTEGER*4 JNOW,HIPIX,NPX,NPY,LPX,LPY,NDIG,MUL,I
C
      SAVE
C
C     ------------------------------------------------------------------
C     DRAWS WINDOW AXES, TICK-MARKS AND AXIS LABELS (PIXEL UNITS)
C     ALSO COMPUTES USER-UNIT TO PIXEL XFORM CONSTANTS (AA & BB)
C     ARGUMENT DEFINITIONS FOLLOW:
C
C     MODE  = 'LIN ' OR 'LOG ' SPECIFIES DISPLAY TYPE
C     ID    = WINDOW ORDINAL NUMBER
C     X1    = X-COOR OF LL-CORNER (USER UNITS)
C     Y1    = Y-COOR OF LL-CORNER (USER UNITS)
C     X2    = X-COOR OF UR-CORNER (USER UNITS)
C     Y2    = Y-COOR OF UR-CORNER (USER UNITS)
C     WPIX  = WIDTH  (PIXELS)
C     HPIX  = HEIGHT (PIXELS)
C
C     IF WINFLC(2,ID) = 'OFF ' SCALE FACTORS ARE COMPUTED BUT AXIS
C                            ARE NOT DRAWN
C     ------------------------------------------------------------------
C
      PXOF=50                 !AXIS X-OFFSET(PIXELS)
      PYOF=15                 !AXIS Y-OFFSET(PIXELS)
C
      WINFLC(4,ID)=MODE
C
      IF(WINFLC(2,ID).EQ.'OFF ') THEN    !TST FOR NO-AXIS MODE
                               PXOF=0
                               PYOF=0
                               ENDIF
C
      DELPX=WPIX-PXOF         !# OF X-PIXELS AVAILABLE FOR PLOT
      DELPY=HPIX-PYOF         !# OF Y-PIXELS AVAILABLE FOR PLOT
C
      PXMX=WPIX
      PYMX=DELPY
C
      PLOTYP(ID)=MODE         !PLOT-TYPE ('LIN ' OR 'LOG ')
C
      DELXV=X2-X1+1.0         !X-RANGE
      DELYV=Y2-Y1+1.0         !Y-RANGE
C
      BB(1,ID)=DELPX/DELXV
      AA(1,ID)=PXOF-BB(1,ID)*X1
C
      IF(MODE.EQ.'LOG ') GO TO 10
C
      BB(2,ID)=-DELPY/DELYV
      AA(2,ID)=DELPY-BB(2,ID)*Y1
C
      GO TO 15
C
   10 BB(2,ID)=-DELPY/(ALOG(Y2)-ALOG(Y1))
      AA(2,ID)=DELPY-BB(2,ID)*ALOG(Y1)
C
   15 IF(WINFLC(2,ID).EQ.'OFF ') RETURN
C
      LGC=GCON(2)             !DRAW AXIS IN FOREGROUND COLOR
C
      CALL XX_DRAWLINE(DPY,WDID(ID),LGC,PXOF,PYMX,PXMX,PYMX)
      CALL XX_DRAWLINE(DPY,WDID(ID),LGC,PXOF,PYMX,PXOF,   0)
C
      CALL TICKL(X1,X2,DELPX,JFIR,JINC,NJ,KFIR,KINC,NK)
C
      DO 20 I=1,NK
      NPX=AA(1,ID)+BB(1,ID)*KFIR+0.5
      CALL XX_DRAWLINE(DPY,WDID(ID),LGC,NPX,PYMX,NPX,PYMX-6)
      KFIR=KFIR+KINC
   20 CONTINUE
C
      DO 40 I=1,NJ
      NPX=AA(1,ID)+BB(1,ID)*JFIR+0.5
      CALL XX_DRAWLINE(DPY,WDID(ID),LGC,NPX,PYMX,NPX,PYMX-10)
C
      CALL NUMASS(JFIR,ANUM,NDIG)
      LPX=NPX+4*NDIG-36
      CALL XX_DRAWSTRING(DPY,WDID(ID),LGC,LPX,PYMX+12,CNUM)
C
      JFIR=JFIR+JINC
   40 CONTINUE
C
      IF(MODE.EQ.'LOG ') GO TO 100
C
      DELPZ=4*DELPY
      CALL TICKL(Y1,Y2,DELPZ,JFIR,JINC,NJ,KFIR,KINC,NK)
C
      DO 50 I=1,NK
      NPY=AA(2,ID)+BB(2,ID)*KFIR+0.5
      CALL XX_DRAWLINE(DPY,WDID(ID),LGC,PXOF,NPY,PXOF+6,NPY)
      KFIR=KFIR+KINC
   50 CONTINUE
C
      DO 60 I=1,NJ
      NPY=AA(2,ID)+BB(2,ID)*JFIR+0.5
      CALL XX_DRAWLINE(DPY,WDID(ID),LGC,PXOF,NPY,PXOF+10,NPY)
C
      CALL NUMASS(JFIR,ANUM,NDIG)
      LPY=NPY+4
      CALL XX_DRAWSTRING(DPY,WDID(ID),LGC,10,LPY,CNUM)
C
      JFIR=JFIR+JINC
   60 CONTINUE
      RETURN
C
  100 CALL LOGTIC(Y1,Y2,JFIR,MUL)
C
      JNOW=MUL*JFIR
  110 IF(JNOW.GT.Y2) RETURN
      NPY=AA(2,ID)+BB(2,ID)*ALOG(FLOAT(JNOW))
      HIPIX=PXOF+6
      IF(MUL.EQ.1) HIPIX=PXOF+10
C
      CALL XX_DRAWLINE(DPY,WDID(ID),LGC,PXOF,NPY,HIPIX,NPY)
C
      IF(MUL.NE.1) GO TO 120
C
      CALL NUMASS(JFIR,ANUM,NDIG)
      LPY=NPY+4
      CALL XX_DRAWSTRING(DPY,WDID(ID),LGC,10,LPY,CNUM)
C
  120 MUL=MUL+1
      JNOW=MUL*JFIR
      IF(MUL.EQ.10) THEN
                    JFIR=JNOW
                    MUL=1
                    ENDIF
      GO TO 110
      END
