C$PROG IOSERR    - Calls IOSMES(IOS) & displays error message
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/16/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE IOSERR(IOS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      INTEGER*4 MSER(20)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(IOS.EQ.0) RETURN
C
      CALL IOSMES(IOS,MSER)
C
      DO 10 I=1,20
      MSSG(I)=MSER(I)
   10 CONTINUE
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
