C$PROG MARKIT2   - Draws a marker at user-coordinates on 2-D plots
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE MARKIT2(IDW,KOLR,KIND,JX,JY)
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
      COMMON/XLDD/ FIGDAT(4,20)
      INTEGER*4    FIGDAT
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
#if (PTRSIZE == 64)
      INTEGER*8    GCOR,   GCON                             !Alpha STAR8
#else
      INTEGER*4    GCOR,   GCON                             !Alpha STAR8
#endif
C     ------------------------------------------------------------------
      INTEGER*4    IDW,JX,JY,H,W
C
      CHARACTER*4  KOLR,KIND
C
      REAL*4       X,Y,XL,XH,YL,YH,A1,B1,A2,B2
C
      INTEGER*4    IX(3),IY(3)
C
      SAVE
C
C     ------------------------------------------------------------------
C     TURNS OFF FULL-WINDOW CURSOR 
C     DRAWS 2-D MARKER OF KIND (KIND) AT CURSOR COORDINATES JX,JY 
C     TURNS ON  FULL-WINDOW CURSOR
C     ------------------------------------------------------------------
C
      XL=WINDAT(5,IDW)
      XH=WINDAT(7,IDW)
      YL=WINDAT(6,IDW)
      YH=WINDAT(8,IDW)
      X=JX
      Y=JY
      A1=AA(1,IDW)
      B1=BB(1,IDW)
      A2=AA(2,IDW)
      B2=BB(2,IDW)
C
      IF(X.LT.XL.OR.X.GT.XH) RETURN
      IF(Y.LT.YL.OR.Y.GT.YH) RETURN
C
      IF(KIND.EQ.'UR  ')     GO TO 10
C
      IX(1)=A1+B1*X
      IX(2)=IX(1)
      IX(3)=A1+B1*XH
      IY(1)=A2+B2*YH
      IY(2)=A2+B2*Y
      IY(3)=IY(2)
      GO TO 20
C
   10 IX(1)=A1+B1*XL
      IX(2)=A1+B1*X
      IX(3)=IX(2)
      IY(1)=A2+B2*Y
      IY(2)=IY(1)
      IY(3)=A2+B2*YL
C
   20 W=FIGDAT(3,IDW)
      H=FIGDAT(4,IDW)
      CALL XX_DRAWLINE(DPY,WDID(IDW),GCOR(5),0,YN(IDW),W,YN(IDW))
      CALL XX_DRAWLINE(DPY,WDID(IDW),GCOR(5),XN(IDW),0,XN(IDW),H)
C
      CALL PLOTPIX(IDW,KOLR,IX,IY,3)
C   
      CALL XX_DRAWLINE(DPY,WDID(IDW),GCOR(5),0,YN(IDW),W,YN(IDW))
      CALL XX_DRAWLINE(DPY,WDID(IDW),GCOR(5),XN(IDW),0,XN(IDW),H)
C
      RETURN
      END
