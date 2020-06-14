C$PROG MARKUM1   - Draws a marker of heignt JHI pixels at user-coor X
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE MARKUM1(IDW,KOLR,X,JHI)
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
      INTEGER*4    IDW,JHI
C
      CHARACTER*4  KOLR
C
      REAL*4       X
C
      INTEGER*4    IX(2),JY(2),H
C
      SAVE
C
C     ------------------------------------------------------------------
C     TURNS OFF FULL-WINDOW CURSOR 
C     DRAWS A MARKER OF HEIGHT JHI PIXELS AT USER-COORDINATE-X
C     TURNS ON  FULL-WINDOW CURSOR
C     ------------------------------------------------------------------
C
      IF(X.LE.WINDAT(5,IDW).OR.X.GE.WINDAT(7,IDW)) RETURN
C
      IX(1)=AA(1,IDW)+BB(1,IDW)*X
      IX(2)=IX(1)
      JY(1)=WINDAT(4,IDW)-16
      JY(2)=JHI
C
      H=FIGDAT(4,IDW)
C
      CALL XX_DRAWLINE(DPY,WDID(IDW),GCOR(5),XN(IDW),0,XN(IDW),H)
C
      CALL PLOTPIX(IDW,KOLR,IX,JY,2)
C   
      CALL XX_DRAWLINE(DPY,WDID(IDW),GCOR(5),XN(IDW),0,XN(IDW),H)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      RETURN
      END
