C$PROG PLOTPIX   - Draws lines specified by pix-coor in IX & JY arrays
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/31/90
C     ******************************************************************
C
      SUBROUTINE PLOTPIX(IDW,KOLR,IX,JY,NX)
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
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
#if (PTRSIZE == 64)
      INTEGER*8    GCO                                      !Alpha STAR8
#else
      INTEGER*4    GCO                                      !Alpha STAR8
#endif
C
      INTEGER*4    IDW,NX,I
C
      CHARACTER*4  KOLR
C
      INTEGER*4    IX(*),JY(*)
C
      INTEGER*2    POINTS(2,512)
C
      SAVE
C
C     ------------------------------------------------------------------
C     DRAW LINES SPECIFIED BY THE PIXEL-COORDINATES IN IX & JY
C     ------------------------------------------------------------------
C
      CALL GETGCO(KOLR,GCO)
C
      DO 10 I=1,NX
      POINTS(1,I)=IX(I)
      POINTS(2,I)=JY(I)
   10 CONTINUE
C
      CALL XX_DRAWLINES(DPY,WDID(IDW),GCO,POINTS,NX,MODE_OR)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      RETURN
      END
