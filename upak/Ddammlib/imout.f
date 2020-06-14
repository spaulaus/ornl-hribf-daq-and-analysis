C$PROG IMOUT     - Determines in x,y-point is out of display or not
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      FUNCTION IMOUT(IDW,X,Y)
C   
C     ------------------------------------------------------------------
      COMMON/PL04/ ILOCX(20),IHICX(20),ILOCY(20),IHICY(20),  !/PL04
     &             KLOCX(20),KHICX(20),KLOCY(20),KHICY(20),  !/PL04
     &             KDDP(20),MINZZ,MAXZZ                      !/PL04
C     ------------------------------------------------------------------
C   
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(X.LT.ILOCX(IDW).OR.X.GT.IHICX(IDW)) GO TO 10
      IF(Y.LT.ILOCY(IDW).OR.Y.GT.IHICY(IDW)) GO TO 10
C   
      IMOUT=0
      RETURN
C   
   10 IMOUT=1
      RETURN
      END
