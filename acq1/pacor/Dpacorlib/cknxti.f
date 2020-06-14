C$PROG CKNXTI
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKNXTI(NXTNDX,NDXNOW)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 NXTNDX(2),NDXNOW
C
      SAVE
C
C     ************************************************************
C     CHECKS TRUE & FALSE BRANCHES & DIS-ALLOWS BACKWARD BRANCHES 
C     ************************************************************
C
      IF(NXTNDX(1).LE.NDXNOW) THEN
      WRITE(CMSSG,10)
   10 FORMAT('ILLEGAL BACKWARD REFERENCE')
      CALL ERRLOG(LOGUT,LOGUP)
                              ENDIF
C
      IF(NXTNDX(2).LE.NDXNOW) THEN
      WRITE(CMSSG,10)
      CALL ERRLOG(LOGUT,LOGUP)
                              ENDIF
C
      RETURN
      END
