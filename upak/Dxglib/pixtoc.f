C$PROG PIXTOC    - Computes user-coor X,Y from pix-vals IX,JY
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE PIXTOC(ID,IX,JY,X,Y)
C
      IMPLICIT NONE
C
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
      INTEGER*4    ID,IX,JY
C
      REAL*4       X,Y
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     COMPUTES USER-COORDINATES (X,Y) FROM PIXEL-VALUES (IX,JY)
C     FOR WINDOW-ID
C     ------------------------------------------------------------------
C
      X=0.0
      Y=0.0
      IF(ID.GT.NUMWIN)      RETURN
      IF(ID.LE.0)           RETURN
      IF(WINFLG(1,ID).EQ.0) RETURN
C
      X=(FLOAT(IX)-AA(1,ID))/BB(1,ID)
C
      Y=(FLOAT(JY)-AA(2,ID))/BB(2,ID)
C
      IF(WINFLC(4,ID).NE.'LOG ') RETURN
C
      Y=EXP(Y)
C
      RETURN
      END
