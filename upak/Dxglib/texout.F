C$PROG TEXOUT    - Outputs line of text to graphics window
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE TEXOUT(IDW,CTEX,KOLR,IX,JY)
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
C
#if (PTRSIZE == 64)
      INTEGER*8   LGC                                       !Alpha STAR8
#else
      INTEGER*4   LGC                                       !Alpha STAR8
#endif
C
      INTEGER*4   IDW,IX,JY
C
      CHARACTER*4 KOLR
C
      CHARACTER*8 CTEX
C
      SAVE
C
C     ------------------------------------------------------------------
C     OUTPUTS THE TEXT CONTAINED IN (CTEX) AT PIX-COOR IX,JY
C     ------------------------------------------------------------------
C
      CALL GETGCO(KOLR,LGC)
C
      CALL XX_DRAWSTRING(DPY,WDID(IDW),LGC,IX,JY,CTEX)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      RETURN
      END
