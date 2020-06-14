C$PROG TAPCLEARO - Clears "serious exception" on output tape
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WTM 04/24/2002
C     ******************************************************************
C
      SUBROUTINE TAPCLEARO(KS,IERR)
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
      COMMON/LM13/ NUMINT,NUMOUT(3),NOUT(3),MTIN(6),MTOUT(6,3),NOSTR
      INTEGER*4    NUMINT,NUMOUT,   NOUT,   MTIN,   MTOUT,     NOSTR
C     ------------------------------------------------------------------
      INTEGER*4    KS,IERR,I
C
      CHARACTER*80 CNAMFD, CTNAME
C
      INTEGER*4    NAMFD(20)
C
      EQUIVALENCE (CNAMFD,NAMFD)
C
      SAVE
C
C     ------------------------------------------------------------------
C     THIS CODE TRIES TO COVER FOR GAPS IN ULTRIX SCSI TAPE DRIVER
C     BY DOING A CLOSE AND REOPEN OF TAPE. THIS APPEARS TO BE NEEDED 
C     CLEAR CERTAIN ERROR CONDITIONS. PROBABLY JUST OPEN IS ENOUGH.
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KS.GT.3.OR.LUCO(KS).LT.0)GO TO 110

      DO 20 I=1,6                           !SAVE DEVICE NAME
      NAMFD(I)=MTOUT(I,KS)
   20 CONTINUE
C
      CALL MT_CLOSE(LUCO(KS))
      CALL MT_OPENRW(NAMFD,LUCO(KS))
      IF(LUCO(KS).LT.0)GO TO 110            !TST FOR ERROR
      RETURN
C
C     ------------------------------------------------------------------
C     SET UP AND SEND ERROR MESSAGES
C     ------------------------------------------------------------------
C
  110 WRITE(CMSSG,115)
  115 FORMAT('AUTO-RECOVER FROM TAPE READ ERROR FAILED')
C
  200 IERR=1
      RETURN
      END
