C$PROG RECLISX   - Displays number of records & files written to output
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE RECLISX(NRRED)
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
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM13/ NUMINT,NUMOUT(3),NOUT(3),MTIN(6),MTOUT(6,3),NOSTR
      INTEGER*4    NUMINT,NUMOUT,   NOUT,   MTIN,   MTOUT,     NOSTR
C     ------------------------------------------------------------------
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF(16384,2,3)
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2    OUBUF
C     ------------------------------------------------------------------
      COMMON/LM15/ LPHED(64),NDX(3)
      INTEGER*4    LPHED,    NDX
C     ------------------------------------------------------------------
      INTEGER*4    NRRED,NUMOUTT(3),NUMINTT,KOUSIZB,KS
C
      SAVE
C
C     ------------------------------------------------------------------
C
      NUMINTT=NUMINT+NRRED
C
      DO 50 KS=1,NOSTR
C
      NUMOUTT(KS)=NUMOUT(KS)+NOUT(KS)
C
      KOUSIZB=2*OUSIZ(KS)
C
      WRITE(CMSSG,10)NRRED,LNBY,NOUT(KS),KOUSIZB,KS
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,20)NUMINTT,NUMOUTT(KS),KS
      CALL MESSLOG(LOGUT,LOGUP)
C
   10 FORMAT(I6,' (',I5,'-B-RECS) READ',I9,' (',I5,'-B-RECS) WRIT',
     &' TO STREAM-',I1)
C
   20 FORMAT(I6,15X,'TOTAL',I9,15X,'TOTAL TO STREAM-',I1)
C
   50 CONTINUE
      RETURN
      END
