C$PROG FI_READ   - Reads one record from simulated-event (evel) file
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE FI_READ(LU,BUF,NBY,NBRED,STAT)
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
      COMMON/LM09/ KINPUT,INRECN
      CHARACTER*4  KINPUT
      INTEGER*4           INRECN
C     ------------------------------------------------------------------
      CHARACTER*4  STAT
C
      INTEGER*4    LU,NBY,NBRED,IOS,I
C
      INTEGER*4    BUF(*),NFW
C
      INTEGER*4    EOFTST
C
      SAVE
C
C     ------------------------------------------------------------------
C
      STAT='GOOD'
      NBRED=0
      NFW=NBY/4
C
      INRECN=INRECN+1
C
      READ(LU,REC=INRECN,IOSTAT=IOS)(BUF(I),I=1,NFW)
C
      IF(IOS.EQ.0)  THEN
                    NBRED=NBY
                    RETURN
                    ENDIF
C
      IF(EOFTST(IOS).NE.0) THEN
                    STAT='EOF '
                    WRITE(CMSSG,10)INRECN
                    GO TO 100
                    ENDIF
C
      STAT='ERR '
C
      WRITE(CMSSG,20)INRECN,IOS
C
   10 FORMAT('END-OF-FILE ON EVENT-FILE AT REC#',I8)
   20 FORMAT('ERROR READING EVENT-FILE - REC#,IOS=',2I8)
C
  100 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END      
