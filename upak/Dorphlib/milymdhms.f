C$PROG MILYMDHMS - Milner's platform specific date & time routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE MILYMDHMS(IAR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      CHARACTER*40 DATE,TIME
C
      INTEGER*4 IAR(6),YR,MO,DA,HR,MN,SEC,MS
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL DATE_AND_TIME(DATE,TIME)
C
      READ(DATE,10)YR,MO,DA
   10 FORMAT(I4,2I2)
C
      READ(TIME,20)HR,MN,SEC,MS
   20 FORMAT(3I2,1X,I3)

      IAR(1)=YR
      IAR(2)=MO
      IAR(3)=DA
      IAR(4)=HR
      IAR(5)=MN
      IAR(6)=SEC
C
      RETURN
      END
