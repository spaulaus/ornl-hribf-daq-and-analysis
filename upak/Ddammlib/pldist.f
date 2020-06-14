C$PROG PLDIST    - Computes distance between line segment and a point
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PLDIST(XA,YA,XB,YB,XP,YP,D,IERR)
C   
C     ------------------------------------------------------------------
C     COMPUTES DISTANCE "D" BETWEEN LINE SEGMENT DEFINED BY
C     (XA,YA - XB,YB) AND THE POINT XP,YP.
C     ------------------------------------------------------------------
C   
      IERR=0                            !RESET ERROR FLAG
C   
C     ------------------------------------------------------------------
C     COMPUTE MIN & MAX VALUES OF X & Y FOR GIVEN LINE SEGMENT
C     ------------------------------------------------------------------
C   
      XMIN=XA
      XMAX=XB
      YMIN=YA
      YMAX=YB
C   
      IF(XA.GT.XB) THEN
                   XMIN=XB
                   XMAX=XA
                   ENDIF
C   
      IF(YA.GT.YB) THEN
                   YMIN=YB
                   YMAX=YA
                   ENDIF
C   
      XMIN=XMIN-0.1
      XMAX=XMAX+0.1
      YMIN=YMIN-0.1
      YMAX=YMAX+0.1
C   
C     ------------------------------------------------------------------
C     TST FOR VERTICAL OR HORIZONTAL LINE SEGMENT
C     ------------------------------------------------------------------
C   
      IF(ABS(XA-XB).LT.0.1) GO TO 100   !TST FOR VERTICAL   LINE SEG
      IF(ABS(YA-YB).LT.0.1) GO TO 200   !TST FOR HORIZONTAL LINE SEG
C   
C     ------------------------------------------------------------------
C     COMPUTE SLOPE & INTERCEPT OF LINE SEG & ORTHOGONAL LINE
C     ------------------------------------------------------------------
C   
      B1=(YB-YA)/(XB-XA)                !SLOPE OF LINE SEG
      A1=YA-B1*XA                       !INTERCEPT OF LINE SEG
C   
      B2=-1.0/B1                        !SLOPE OF ORTHOGONAL LINE
      A2=YP-B2*XP                       !INTERCEPT OF ORTHOGONAL LINE
C   
      XI=(A2-A1)/(B1-B2)                !X-INTERSECTION OF LINES
      YI=A1+B1*XI                       !Y-INTERSECTION OF LINES
C   
      IF(XI.LT.XMIN) GO TO 300          !TST FOR WITHIN LIMITS
      IF(XI.GT.XMAX) GO TO 300
      IF(YI.LT.YMIN) GO TO 300
      IF(YI.GT.YMAX) GO TO 300
C   
      D=SQRT((XP-XI)**2+(YP-YI)**2)     !DIST BETWEEN POINT AND LINE
C   
      RETURN
C   
  100 IF(YP.LT.YMIN) GO TO 300          !VERTICAL LINE SEGMENT
      IF(YP.GT.YMAX) GO TO 300          !TST FOR WITHIN LIMITS
C   
      D=ABS(XP-XA)                      !DIST BETWEEN POINT AND LINE
      RETURN
C   
  200 IF(XP.LT.XMIN) GO TO 300          !HORIZONTAL LINE SEGMENT
      IF(XP.GT.XMAX) GO TO 300          !TST FOR WITHIN LIMITS
C   
      D=ABS(YP-YA)                      !DIST BETWEEN POINT AND LINE
      RETURN
C   
  300 IERR=1                            !ERROR - OUT OF SEGMENT RANGE
      RETURN
      END
