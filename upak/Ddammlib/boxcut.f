C$PROG BOXCUT    - Determine box intersections
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BOXCUT(IDW,XA,YA,XB,YB,XO,YO,IFAIL)
C   
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
C
      CHARACTER*4  IFAIL
C
      REAL*4 X(4),Y(4)
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      XMIN=WINDAT(5,IDW)
      YMIN=WINDAT(6,IDW)
      XMAX=WINDAT(7,IDW)
      YMAX=WINDAT(8,IDW)
C   
      IFAIL='YES '
C     ------------------------------------------------------------------
C     TEST FOR BOTH POINTS OUTSIDE AND IN SAME REGION
C     ------------------------------------------------------------------
C   
      IF(XA.LT.XMIN.AND.XB.LT.XMIN) RETURN
      IF(XA.GT.XMAX.AND.XB.GT.XMAX) RETURN
      IF(YA.LT.YMIN.AND.YB.LT.YMIN) RETURN
      IF(YA.GT.YMAX.AND.YB.GT.YMAX) RETURN
C   
C     ------------------------------------------------------------------
C     TEST FOR VERTICAL OR HORIZONTAL LINE
C     ------------------------------------------------------------------
C   
      IF(YA.EQ.YB) GO TO 100
      IF(XA.EQ.XB) GO TO 200
C   
C     ------------------------------------------------------------------
C     CALCULATE INTERSECTION POINTS WITH BOX BOUNDRY LINES
C     ------------------------------------------------------------------
C   
      BB=(YB-YA)/(XB-XA)
C   
      X(1)=XA+(YMIN-YA)/BB
      Y(1)=YMIN
C   
      X(2)=XA+(YMAX-YA)/BB
      Y(2)=YMAX
C   
      X(3)=XMIN
      Y(3)=YA+BB*(XMIN-XA)
C   
      X(4)=XMAX
      Y(4)=YA+BB*(XMAX-XA)
C   
      XLO=100000000.0
      JLO=0
      DO 10 I=1,4
      IF(X(I).LE.XA)   GO TO 10
      IF(X(I).LT.XMIN) GO TO 10
      IF(X(I).GT.XMAX) GO TO 10
      IF(Y(I).LT.YMIN) GO TO 10
      IF(Y(I).GT.YMAX) GO TO 10
      IF(X(I).GT.XLO)  GO TO 10
      XLO=X(I)
      JLO=I
   10 CONTINUE
      IF(JLO.EQ.0) RETURN
      IFAIL='NO  '
      XO=X(JLO)
      YO=Y(JLO)
      RETURN
C   
C   
C     ------------------------------------------------------------------
C     HORIZONTAL LINE CASE
C     ------------------------------------------------------------------
C   
  100 IF(YA.LT.YMIN) RETURN
      IF(YA.GT.YMAX) RETURN
      IF(XA.GT.XMAX) RETURN
      XO=XMIN
      IF(XA.GE.XMIN) XO=XMAX
      YO=YA
      IFAIL='NO  '
      RETURN
C   
C     ------------------------------------------------------------------
C     VERTICAL LINE CASE
C     ------------------------------------------------------------------
C   
  200 IF(XA.LT.XMIN .OR.XA.GT.XMAX) RETURN
      IF(YA.GT.YMAX.AND.YB.GT.YMAX) RETURN
      IF(YA.LT.YMIN.AND.YB.LT.YMIN) RETURN
C   
      IF(YB.GT.YA) GO TO 220
C   
      IF(YA.GE.YMAX) YO=YMAX
      IF(YA.LT.YMAX) YO=YMIN
      GO TO 230
C   
  220 IF(YA.LT.YMIN) YO=YMIN
      IF(YA.GE.YMIN) YO=YMAX
C   
  230 XO=XA
      IFAIL='NO  '
      RETURN
      END
