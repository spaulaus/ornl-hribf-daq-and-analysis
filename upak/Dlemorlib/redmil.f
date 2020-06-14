C$PROG REDMIL    - Reads one record from the MIL-file
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE REDMIL(IREC,BUF)
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
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      INTEGER*4    IREC,IOS
C
      INTEGER*2    BUF(128)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      READ(LML,REC=IREC,IOSTAT=IOS)BUF
      IF(IOS.NE.0) GO TO 20
      RETURN
C
   20 WRITE(CMSSG,30)IOS
   30 FORMAT('ERROR READING IN MIL-FILE - IOS =',I4)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL EXITOR(1)
      END
