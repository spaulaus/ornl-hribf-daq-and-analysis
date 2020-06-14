C$PROG LABLOUT   - Outputs text ap specified pixel coordinate
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE LABLOUT(IDW,CTEX,KOLR,IX,JY,NCH)
C
      IMPLICIT INTEGER*4 (A-Z)
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
#if (PTRSIZE == 64)
      INTEGER*8    LGC                                      !Alpha STAR8
#else
      INTEGER*4    LGC                                      !Alpha STAR8
#endif
C
      CHARACTER*4  KOLR
C
      INTEGER*4    IDW,IX,JY,NCH,I
C
      CHARACTER*80 CTEX,CTMP
C
      CHARACTER*1 ITMP(80),BLANK
C
      EQUIVALENCE (CTMP,ITMP)
C
      SAVE
C
C     ------------------------------------------------------------------
C     OUTPUTS THE TEXT CONTAINED IN (CTEX) AT PIX-COOR IX,JY
C     ------------------------------------------------------------------
C
      BLANK=' '
      CTMP=CTEX
      NCH=0
C
      DO 10 I=1,80
      IF(ITMP(I).EQ.BLANK) GO TO 10
      NCH=I
   10 CONTINUE
C
      CALL GETGCO(KOLR,LGC)
C
      CALL XX_DRAWSTRING(DPY,WDID(IDW),LGC,IX,JY,CTEX)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      RETURN
      END
