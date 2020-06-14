C$PROG MILTIME   - Milner's time routine - returns 8 bytes ASCII
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE MILTIME(JAR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    IAR(6),YR,MO,DA,HR,MN,SC
C
      CHARACTER*8  JAR
C
      EQUIVALENCE (YR,IAR(1)),
     &            (MO,IAR(2)),
     &            (DA,IAR(3)),
     &            (HR,IAR(4)),
     &            (MN,IAR(5)),
     &            (SC,IAR(6))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL MILYMDHMS(IAR)
C
      WRITE(JAR,10)HR,MN,SC
   10 FORMAT(I2.2,':',I2.2,':',I2.2)
C
      RETURN
      END
