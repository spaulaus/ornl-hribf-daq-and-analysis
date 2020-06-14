C$PROG NUCUR     - Produces a full-window cursor
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE NUCUR
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
      COMMON/XLII/ LASWIN,LWIN,WNDX,IVX,IVY
      INTEGER*4    LASWIN,LWIN,WNDX,IVX,IVY
C     ------------------------------------------------------------------
#if (PTRSIZE == 64)
      INTEGER*8    LWIN8                                    !Alpha STAR8
#else
      INTEGER*4    LWIN8                                    !Alpha STAR8
#endif
C
      INTEGER*4    LIVX,LIVY,W,H,I
C
      INTEGER*4    LX(20),LY(20)
C
      EQUIVALENCE (LX,XN),(LY,YN)
C
      SAVE
C
C     ------------------------------------------------------------------
C     PRODUCES A FULL-WINDOW CURSOR - CALLED BY XX_EVENTMAN
C     ------------------------------------------------------------------
C
      IF(LWIN.EQ.LASWIN) THEN
      IF(IVX.EQ.LIVX.AND.IVY.EQ.LIVY) RETURN
      GO TO 50
                         ENDIF
C
      DO 10 I=1,NUMWIN
      IF(LWIN.EQ.WDID(I)) GO TO 20
   10 CONTINUE
      RETURN
C
   20 WNDX=I
      W=FIGDAT(3,WNDX)
      H=FIGDAT(4,WNDX)
      LASWIN=LWIN
      LWIN8=LWIN
C
      IF(NUWIN(WNDX).EQ.'YES ') GO TO 100
C
   50 CALL XX_DRAWLINE(DPY,LWIN8,GCOR(5),0,LY(WNDX),W,LY(WNDX))
      CALL XX_DRAWLINE(DPY,LWIN8,GCOR(5),LX(WNDX),0,LX(WNDX),H)
C
  100 CALL XX_DRAWLINE(DPY,LWIN8,GCOR(5),0,IVY,W,IVY)
      CALL XX_DRAWLINE(DPY,LWIN8,GCOR(5),IVX,0,IVX,H)
      LX(WNDX)=IVX
      LY(WNDX)=IVY
      LIVX=IVX
      LIVY=IVY
      NUWIN(WNDX)='NO  '
      RETURN
      END
