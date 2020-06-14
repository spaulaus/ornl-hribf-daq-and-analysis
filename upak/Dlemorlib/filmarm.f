C$PROG FILMARM   - Writes NEOF file-marks & backs up NBAK file-marks
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE FILMARM(NEOF,NBAK,IERR)
C
      IMPLICIT NONE
C
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
      INTEGER*4    NEOF,NBAK,IERR,KERR,KS
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 KS=1,NOSTR
      CALL FILMAR(LUCO(KS),NEOF,NBAK,IERR)
   10 CONTINUE
C
      RETURN
      END
