C$PROG FILCIR    - Draws filler circle
C
      SUBROUTINE FILLCIR(IDW,GCO,IX,JY,IWID,IHI)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      INTEGER*4    DPY,WDID,GCO                         !STAR-8 on Alpha
C     ------------------------------------------------------------------
C
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
C
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL XX_FILLARC(DPY,
     &                WDID(IDW),
     &                GCO,
     &                IX,
     &                JY,
     &                IWID,
     &                IHI,
     &                0,
     &                23040)
C
      RETURN
      END
