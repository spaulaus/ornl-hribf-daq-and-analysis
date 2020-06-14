C$PROG PLOB      - Plots (displays) background
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PLOB(IDW,MODE)
C   
C     ------------------------------------------------------------------   
      COMMON/SM07/ WINL(4),KSCAL,LASIDW
C     ------------------------------------------------------------------
      COMMON/SM09/ XYP(50,2),NBXY
C     ------------------------------------------------------------------
C   
      DIMENSION X(50),Y(50),XL(50),YL(50),INO(50)
C   
      EQUIVALENCE (XL(1),XYP(1,1)),(YL(1),XYP(1,2))
C   
      EQUIVALENCE (XMIN,WINL(1)),(XMAX,WINL(2)),
     &            (YMIN,WINL(3)),(YMAX,WINL(4))
C   
      DATA NXY/0/
C
      CHARACTER*4  MODE,KOL,IFAIL
C
      SAVE
C   
C     ------------------------------------------------------------------
C     XYP(J,1)  = NEW X-COOR OF JTH BGD POINT (CHAN# UNITS)
C     XYP(J,2)  = NEW Y-COOR OF JTH BGD POINT ("COUNTS")
C     NBXY      = NEW # OF ENTRIES IN LISTS
C   
C     X(J)      = OLD X-COOR OF JTH BGD POINT (CHAN#)
C     Y(J)      = OLD Y-COOR OF JTH BGD POINT (CHAN#)
C     NXY       = OLD # OF ENTRIES IN LISTS
C   
C     MODE = "INIT" SAYS ZOT  OLD LIST (I.E. SET NXY=0)
C     MODE = 'ERAS' SAYS PLOT OLD LIST BLACK & SET NXY=0
C     MODE = 'PLOT' SAYS COPY NEW LIST TO OLD LIST & PLOT
C     ------------------------------------------------------------------
C   
      KOL='OGRE'
C   
      IF(MODE.EQ.'INIT') THEN
                         NXY=0
                         RETURN
                         ENDIF
C   
      IF(MODE.EQ.'ERAS') GO TO 200
C   
      IF(MODE.EQ.'PLOT') GO TO 10
C   
C     ------------------------------------------------------------------
C     SET FLAGS FOR X-Y POINTS INSIDE OR OUTSIDE OF BOX
C     ------------------------------------------------------------------
C   
   10 DO 20 I=1,NBXY
      P=XL(I)
      Q=YL(I)
      INO(I)=0
      IF(P.LT.XMIN.OR.P.GT.XMAX) GO TO 20
      IF(Q.LT.YMIN.OR.Q.GT.YMAX) GO TO 20
      INO(I)=1
   20 CONTINUE
C   
C     ------------------------------------------------------------------
C     PICK UP FIRST POINT AND TEST FOR INSIDE OR OUTSIDE
C     ------------------------------------------------------------------
C   
      N=0
      K=1
      IF(K.GT.NBXY)   GO TO 100      !TST FOR DONE
      IF(INO(K).EQ.0) GO TO 50       !TST FOR OUTSIDE BOX
C   
   40 N=N+1                          !POINT INSIDE BOX - SAVE IT
      X(N)=XL(K)
      Y(N)=YL(K)
      K=K+1                          !INC POINT CNTR
      IF(K.GT.NBXY)   GO TO 100      !TST FOR DONE
      IF(INO(K).NE.0) GO TO 40       !TST FOR STILL INSIDE
C   
      CALL BOXCUT(IDW,X(N),Y(N),XL(K),YL(K),XO,YO,IFAIL)
C   
      N=N+1                          !OTHERWISE SAVE EXIT POINT
      X(N)=XO
      Y(N)=YO
      IF(K.GE.NBXY) GO TO 100
C   
   50 XX=XL(K)                       !THIS POINT OUTSIDE
      YY=YL(K)
      K=K+1                          !GET NEXT POINT
      IF(K.GT.NBXY)   GO TO 100      !TST FOR DONE
C   
      CALL BOXCUT(IDW,XX,YY,XL(K),YL(K),XO,YO,IFAIL)
C   
      IF(XO.EQ.XMAX.AND.K.LT.NBXY) GO TO 50
C   
      IF(IFAIL.EQ.'NO  ') GO TO 60   !TST FOR BOX CUT
      GO TO 50                       !IF NO, KEEP TRYING
C   
   60 N=N+1                          !SAVE BOX CUT POINT
      X(N)=XO
      Y(N)=YO
      IF(INO(K).EQ.1) GO TO 40       !TST FOR INSIDE
C   
      CALL BOXCUT(IDW,X(N),Y(N),XL(K),YL(K),XO,YO,IFAIL)
C   
      IF(IFAIL.EQ.'YES ') GO TO 50   !TST FOR BOX-CUT
      N=N+1
      X(N)=XO
      Y(N)=YO
      GO TO 50
C   
  100 NXY=N
      DXX=0.005*(XMAX-XMIN)
      IF(X(NXY).EQ.XMAX) X(NXY)=X(NXY)-DXX
C   
C     ------------------------------------------------------------------
C     PLOT THE MODIFIED LIST IF ANY
C     ------------------------------------------------------------------
C   
  200 IF(NXY.LT.1)          RETURN
      IF(X(NXY).LE.WINL(1)) RETURN
      IF(X(1)  .GE.WINL(2)) RETURN
C   
      CALL PLOTXY(IDW,KOL,X,Y,NXY)
C   
      RETURN
      END
