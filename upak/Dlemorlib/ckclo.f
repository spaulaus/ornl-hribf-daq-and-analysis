C$PROG CKCLO     - Check & close output tape units
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CKCLO(KMD,KL)
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
      INTEGER*4    KL,IERR
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(LOUO(KL).NE.'YES ') RETURN              !TST FOR OPEN
C
      IF(KFO(KL).EQ.'TAPE'.AND.KMD.EQ.'DMUL')THEN
            CALL MT_REWUL(LUCO(KL),IERR)
            IF(IERR.NE.0)CALL IOSERR(IERR)
C           CALL TMOU(KMD,MTOUT(1,KL),IERR)
      ENDIF
      CLOSE(UNIT=LOU(KL))                        !CLOSE LU - OPEN OR NOT
      CALL SYS_CLOSE(LUCO(KL))
      LUCO(KL)=-1                                !SET CHANNEL TO -1 
      LOUO(KL)='NO  '                            !SET CLOSED FLAG
C
C
      RETURN
      END
