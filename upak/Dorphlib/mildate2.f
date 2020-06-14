C$PROG MILDATE2  - Milner's date routine - returns 12-bytes ASCII (YR2)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE MILDATE2(JAR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    MON(12),IAR(6),YR,MO,DA,HR,MN,SC
      CHARACTER*4  cMON(12)
      EQUIVALENCE  (cMON, MON)
C
      CHARACTER*12 JAR
C
      DATA         cMON/'Jan-','Feb-','Mar-','Apr-','May-','Jun-',
     &                 'Jul-','Aug-','Sep-','Oct-','Nov-','Dec-'/
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
      YR=YR-2000
C
      IF(YR.LT.0)  YR=0
C
      IF(YR.GT.99) YR=99
C
      WRITE(JAR,10)DA,MON(MO),YR
   10 FORMAT(I2.2,'-',A4,I2.2,'   ')
C
      RETURN
      END
