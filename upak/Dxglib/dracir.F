C$PROG DRACIR    - Draws small circles
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE DRACIR(IDW,KOLR,IX,JY)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
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
      INTEGER*8    GCO                                      !Alpha STAR8
#else
      INTEGER*4    GCO                                      !Alpha STAR8
#endif
C
      INTEGER*4    IDW,KOLR,IX,JY
C
      SAVE
C     ------------------------------------------------------------------
C
      CALL GETGCO(KOLR,GCO)
C
      CALL XX_DRAWARC(DPY,
     &                WDID(IDW),
     &                GCO,
     &                IX-3,
     &                JY-3,
     &                7,
     &                7,
     &                0,
     &                23040)
C
      RETURN
      END
