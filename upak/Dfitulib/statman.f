C$PROG STATMAN   - Displays/logs status information
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/22/2000
C     ******************************************************************
C
      SUBROUTINE STATMAN
C
      IMPLICIT NONE 
C
C     ------------------------------------------------------------------
      INTEGER*4 MXDAT
C
      PARAMETER (MXDAT=500)
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/FT08/ NAMF(20)
      INTEGER*4    NAMF
C     ------------------------------------------------------------------
      COMMON/FT09/ SKIPF(MXDAT),TCHISQ
      INTEGER*4    SKIPF
      REAL*8                    TCHISQ
C     ------------------------------------------------------------------
      COMMON/FT10/ KINDUU,KINDU,USUN,ALUN,UMUL
      INTEGER*4    KINDUU,KINDU
      REAL*8                    USUN,ALUN,UMUL
C     ------------------------------------------------------------------
      INTEGER*4    STRLEN
C
      CHARACTER*80 CNAMF
      EQUIVALENCE (CNAMF,NAMF)
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     KINDUU= 1 SAYS ALL UNCERT = ALUN (%)
C     KINDUU= 2 SAYS UNSPECIFIED UNCERT = USUN (%)
C     KINDU = 1 SAYS COMPUTE UNCERT AS COUNTING STATISTICS
C     KINDU = 2 SAYS UNCERT GIVEN BY "UIN" IS ABSOLUTE
C     KINDU = 3 SAYS UNCERT GIVEN BY "UIN" IS IN %
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,110)CNAMF(1:STRLEN(CNAMF))
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,115)
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(KINDUU.EQ.1) THEN
      WRITE(CMSSG,120)ALUN
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      ENDIF
C
      IF(KINDU.EQ.1) THEN
      WRITE(CMSSG,130)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(KINDU.EQ.2) THEN
      WRITE(CMSSG,140)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(KINDU.EQ.3) THEN
      WRITE(CMSSG,150)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      WRITE(CMSSG,160)USUN
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,170)UMUL
      CALL MESSLOG(LOGUT,LOGUP)
C
  110 FORMAT('Input file is ',A)
  115 FORMAT(12('-----'))
  120 FORMAT('All uncertainties to be set to -------------',F8.3,'%')
  130 FORMAT('Compute uncertainties as counting statistics')
  140 FORMAT('Given uncertainties in absolute   units')
  150 FORMAT('Given uncertainties in percentage units')
  160 FORMAT('Unspecified uncertainties are set to -------',F8.3,'%')
  170 FORMAT('Uncertainty multiplier is set to -----------',F8.3)
      RETURN
      END
