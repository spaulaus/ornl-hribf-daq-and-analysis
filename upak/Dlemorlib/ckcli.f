C$PROG CKCLI     - Check & close input tape unit
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CKCLI(KMD)
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
      CHARACTER*4  KMD
C
      INTEGER*4    IERR
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(LINO.NE.'YES ') RETURN                  !TST FOR OPEN
C
      IF(KFI.EQ.'TAPE'.AND.KMD.EQ.'DMUL')THEN
          CALL MT_REWUL(LUCI,IERR)
          CALL IOSERR(IERR)
C         CALL TMOU(KMD,MTIN,IERR)
      ENDIF
      CLOSE(UNIT=LIN)                            !CLOSE LU - OPEN OR NOT
      CALL SYS_CLOSE(LUCI)
      LUCI=-1                                    !SET CHANNEL TO 0
      LINO='NO  '                                !SET CLOSED FLAG
C
      RETURN
      END
