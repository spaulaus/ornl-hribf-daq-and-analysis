C$PROG PLOTYY    - Plots data in Y-array in either hist- or vector mode
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE PLOTYY(IDW,MODE,KOLR,KRUD,X1,XS,Y,NX)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/XLBB/ AA(2,20),BB(2,20),PLOTYP(20)
      REAL*4       AA,      BB
      CHARACTER*4                    PLOTYP
C     ------------------------------------------------------------------
      COMMON/XLGG/ IX(512),JY(512)
      INTEGER*4    IX,     JY
C     ------------------------------------------------------------------
      INTEGER*4    IDW,KRUD,NX
C
      CHARACTER*4  MODE,KOLR,PLTY
C   
      REAL*4       Y(*),X1,XS,XSS
C
      REAL*4       AAX,BBX,AAY,BBY,ALOG,XX,YY,YSUM
C
      INTEGER*4    NDO,NN,I,J,K
C
      SAVE
C
C     ------------------------------------------------------------------
C     PLOTS THE DATA IN ARRAY-Y IN EITHER HIST- OR VECT-MODE
C     
C     IDW  =  WINDOW ID
C     MODE =  'HIST' OR 'VECT'
C     KOLR =  COLOR SPECIFICATION
C     X1   =  FIRST X-COORDINATE
C     XS   =  X-STEP (USUALLY 1.0)
C     Y    -  CONTAINS Y-COORDINATES
C     NX   =  NUMBER OF POINTS
C     ************************************************************
C
      AAX=AA(1,IDW)
      BBX=BB(1,IDW)
      AAY=AA(2,IDW)
      BBY=BB(2,IDW)
      PLTY=PLOTYP(IDW)
C
      IF(MODE.EQ.'HIST') GO TO 100
C
C     ************************************************************
C     PLOT IN VECTOR-MODE
C     ************************************************************
C   
      NN=0
      XX=X1
C
      DO 20 I=1,NX
      NN=NN+1
      IX(NN)=AAX+BBX*XX
C
      IF(PLTY.NE.'LOG ') JY(NN)=AAY+BBY*Y(I)
      IF(PLTY.EQ.'LOG ') THEN
                         YY=Y(I)
                         IF(YY.LT.1.0) YY=0.8
                         JY(NN)=AAY+BBY*ALOG(YY)
                         ENDIF
C
      XX=XX+XS
      IF(NN.LT.500) GO TO 20
C
      CALL PLOTPIX(IDW,KOLR,IX,JY,NN)
C
      IF(MSGF.NE.'    ') RETURN
C
      IX(1)=IX(NN)
      JY(1)=JY(NN)
      NN=1
   20 CONTINUE
      IF(NN.LT.2) RETURN
      CALL PLOTPIX(IDW,KOLR,IX,JY,NN)
      RETURN
C   
C     ************************************************************
C     PLOT IN HIST-MODE
C     ************************************************************
C
  100 IF(KRUD.GT.1) GO TO 200
C
      NN=0
      XX=X1-0.5*XS
C   
      DO 120 I=1,NX
C   
      NN=NN+1
      IX(NN)=AAX+BBX*XX
C
      IF(PLTY.NE.'LOG ') JY(NN)=AAY+BBY*Y(I)
      IF(PLTY.EQ.'LOG ') THEN
                         YY=Y(I)
                         IF(YY.LT.1.0) YY=0.8
                         JY(NN)=AAY+BBY*ALOG(YY)
                         ENDIF
C
      NN=NN+1
      XX=XX+XS
      IX(NN)=AAX+BBX*XX
      JY(NN)=JY(NN-1)
C   
      IF(NN.LT.500) GO TO 120
C   
      CALL PLOTPIX(IDW,KOLR,IX,JY,NN)
C   
      IF(MSGF.NE.'    ') RETURN
C   
      IX(1)=IX(NN)
      JY(1)=JY(NN)
      NN=1
C   
  120 CONTINUE
C
      IF(NN.LT.2) RETURN
      CALL PLOTPIX(IDW,KOLR,IX,JY,NN)
      RETURN
C   
C     ************************************************************
C     PLOT IN HIST-MODE WITH CRUNCHED DISPLAY
C     ************************************************************
C
  200 NN=0
      XSS=KRUD*XS
      XX=X1-0.5*XS
      NDO=NX/KRUD
      IF(NDO.LT.2) RETURN
      K=0
C   
      DO 300 I=1,NDO
C
      YSUM=0.0
      DO 210 J=1,KRUD
      K=K+1
      YSUM=YSUM+Y(K)
  210 CONTINUE
      YSUM=YSUM/FLOAT(KRUD)
C   
      NN=NN+1
      IX(NN)=AAX+BBX*XX
C
      IF(PLTY.NE.'LOG ') JY(NN)=AAY+BBY*YSUM
      IF(PLTY.EQ.'LOG ') THEN
                         YY=YSUM
                         IF(YY.LT.1.0) YY=0.8
                         JY(NN)=AAY+BBY*ALOG(YY)
                         ENDIF
C
      NN=NN+1
      XX=XX+XSS
      IX(NN)=AAX+BBX*XX
      JY(NN)=JY(NN-1)
C   
      IF(NN.LT.500) GO TO 300
C   
      CALL PLOTPIX(IDW,KOLR,IX,JY,NN)
C   
      IF(MSGF.NE.'    ') RETURN
C   
      IX(1)=IX(NN)
      JY(1)=JY(NN)
      NN=1
C   
  300 CONTINUE
C
      IF(NN.LT.2) RETURN
      CALL PLOTPIX(IDW,KOLR,IX,JY,NN)
      RETURN
      END
