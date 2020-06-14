C$PROG DUBLIV    - 2-D interpolation routine
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      FUNCTION DUBLIV(XP,YP,ZVAL,XV,YV,MX,MY,MAXX)
C
      DIMENSION XP(1),YP(1),ZVAL(1),CZ(3)
C
C     ------------------------------------------------------------------
C     THIS ROUTINE DOES A DOUBLE INTERPOLATION ON A TWO DIMENSIONAL
C     TABLE OF Z VS X AND Y.
C     XV AND YV IS THE POINT AT WHICH Z IS EVALUATED.
C
C     DETERMINE ENTRIES IN TABLE WHICH ARE TO BE USED BY FINDING
C     VALUES OF XP(J) AND YP(J) WHICH ARE LARGER THAN XV AND YV
C     ------------------------------------------------------------------
C
      MX1=MX-1
      MY1=MY-1
      DO 10 I=2,MX1
      IF(XP(I).GT.XV) GO TO 20
   10 CONTINUE
      I=MX1
   20 IX2=I
      IX1=IX2-1
      IX3=IX2+1
      DO 30 I=2,MY1
      IF(YP(I).GT.YV) GO TO 40
   30 CONTINUE
      I=MY1
   40 JY2=I
      JY1=JY2-1
      JY3=JY2+1
      X1=XP(IX1)
      X2=XP(IX2)
      X3=XP(IX3)
C
C     ------------------------------------------------------------------
C     X(IX1),X(IX2),X(IX3) AND Y(JY1),Y(JY2),Y(JY3)
C
C     AND ((ZVAL(I1,J1),I1=IX1,IX3),J1=JY1,JY3) ARE THE X,Y, AND
C
C     Z VALUES IN THE TABLE TO BE FIT.
C     MAKE THREE FITS TO THE VALUES IN THE TABLE
C     FIT THE POINTS (X(IX1),ZVAL(IX1,JY1),(X(IX2),ZVAL(IX2,JY1))
C     (X(IX3),ZVAL(IX3,JY1)) AND THEN FOR JY2 AND JY3.
C     ------------------------------------------------------------------
C
      K=0
      DO 50 J=JY1,JY3
      K=K+1
      J1=INDXF(IX1,J,MAXX)
      J2=INDXF(IX2,J,MAXX)
      J3=INDXF(IX3,J,MAXX)
      CZ(K)=FIT3(X1,X2,X3,ZVAL(J1),ZVAL(J2),ZVAL(J3),XV)
   50 CONTINUE
C
C     ------------------------------------------------------------------
C     NOW WE HAVE THREE VALUES CZ(K),K=1,3
C     CORRESPONDING TO ZVALS AT (XV,Y(JY1))
C     (XV,Y(JY2)) AND XV(,Y(JY3)) NOW DO FIT
C     ON Y PARAMETER TO GET ZVAL AT (XV,YV)
C     ------------------------------------------------------------------
C
      DUBLIV=FIT3(YP(JY1),YP(JY2),YP(JY3),CZ(1),CZ(2),CZ(3),YV)
C
      RETURN
      END
