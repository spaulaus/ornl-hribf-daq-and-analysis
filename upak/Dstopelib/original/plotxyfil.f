C$PROG PLOTXYFIL - Displays list of X,Y-points as (filled circles)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE PLOTXYFIL(IDW,KOLR,X,Y,NX)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
C     ------------------------------------------------------------------
      COMMON/XLBB/ AA(2,20),BB(2,20),PLOTYP(20)
      REAL*4       AA,      BB
      CHARACTER*4                    PLOTYP
C     ------------------------------------------------------------------
      COMMON/XLGG/ IX(512),JY(512)
      INTEGER*4    IX,     JY
C     ------------------------------------------------------------------
      INTEGER*4    GCO                                      !Alpha STAR8
C
      CHARACTER*4  PLTY,KOLR
C
      REAL*4       X(*),Y(*)
C
      REAL*4       AAX,BBX,AAY,BBY,ALOG,YY
C
      SAVE
C
C     ------------------------------------------------------------------
C
      AAX=AA(1,IDW)
      BBX=BB(1,IDW)
      AAY=AA(2,IDW)
      BBY=BB(2,IDW)
      PLTY=PLOTYP(IDW)
      NN=0
C
      CALL GETGCO(KOLR,GCO)
C
      DO 20 I=1,NX
C
      IF(Y(I).LE.0.0) GO TO 20
C
      NN=NN+1
      IX(NN)=AAX+BBX*X(I)
C
      IF(PLTY.NE.'LOG ') JY(NN)=AAY+BBY*Y(I)
      IF(PLTY.EQ.'LOG ') THEN
                         YY=Y(I)
                         IF(YY.LT.1.0) YY=1.0
                         JY(NN)=AAY+BBY*ALOG(YY)
                         ENDIF
C
      CALL XX_FILLARC(DPY,
     &                WDID(IDW),
     &                GCO,
     &                IX(NN)-3,
     &                JY(NN)-3,
     &                7,
     &                7,
     &                0,
     &                23040)
C
      IF(NN.LT.500) GO TO 20
C
      IF(MSGF.NE.'    ') GO TO 50
C
      IX(1)=IX(NN)
      JY(1)=JY(NN)
      NN=1
   20 CONTINUE
C
   50 CALL XX_SYNC(DPY,.TRUE.)
      RETURN
      END
