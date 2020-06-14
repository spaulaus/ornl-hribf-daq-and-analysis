C$PROG DRRSUB    - Dummy DRRSUB routine
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 02/17/99
C     ******************************************************************
C
      SUBROUTINE DRRSUB(IEXIST)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SC02/ NAMH(20)
      INTEGER*4    NAMH
C     ------------------------------------------------------------------
      INTEGER*4    IEXIST,LENG
C
      CHARACTER*80 CNAMH,HISNAME
C   
      EQUIVALENCE (CNAMH,NAMH)
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      CALL WHITE_OUT(CNAMH,HISNAME,LENG)
C
      IF(IEXIST.EQ.0)THEN
         CALL WHITE_OUT(CNAMH,HISNAME,LENG)
         CMSSG=HISNAME
         CMSSG(LENG+1:)=
     &    '.drr does not exist, and no DRRSUB was provided.'
         CALL MESSLOG(LOGUT,LOGUP)
         CMSSG=
     &    'I cannot go on this way! You will have to try again.'
         CALL MESSLOG(LOGUT,LOGUP)
         STOP
      ELSE
         CMSSG=
     &   'Will use existing HIS & DRR files, since no DRRSUB provided.'
         CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
      RETURN
C   
      END
