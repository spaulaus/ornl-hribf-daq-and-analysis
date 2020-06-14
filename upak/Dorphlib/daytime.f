C$PROG DAYTIME   - Returns Date & Time as integers & ASCII string
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE DAYTIME(YR4,MO,DA,HR,MN,SEC,DATIM)
C
      IMPLICIT NONE
C
      CHARACTER*20 DATIM
C
      INTEGER*4    YR4,YR,MO,DA,HR,MN,SEC,HS
C
      CHARACTER*3  MONAM(12)
C
      DATA         MONAM/'Jan','Feb','Mar','Apr','May','Jun',
     &                   'Jul','Aug','Sep','Oct','Nov','Dec'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL TIMEDATE(YR,MO,DA,HR,MN,SEC,HS)
C
      YR4=YR+1900
C
      IF(YR.LT.38) YR4=YR+2000
C
      WRITE(DATIM,20)DA,MONAM(MO),YR4,HR,MN,SEC
   20 FORMAT(I2.2,'-',A,'-',I4.4,' ',I2.2,':',I2.2,':',I2.2)
C
      RETURN
C
      END
