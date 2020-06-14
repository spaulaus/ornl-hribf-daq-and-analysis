C$PROG READUM1   - Reads one record from input tape or evel-file
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE READUM1(C,IBUF,NBY,NBRED,STAT)
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
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM09/ KINPUT,INRECN
      CHARACTER*4  KINPUT
      INTEGER*4           INRECN
C     ------------------------------------------------------------------
      INTEGER*4    IBUF(*),C,NBY,NBRED,STAT
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KINPUT.EQ.'FILE') GO TO 100
C
      CALL READUM(C,IBUF,NBY,NBRED,STAT)
      RETURN
C
  100 CALL FI_READ(LIN,IBUF,LNBY,NBRED,STAT)
      RETURN
      END
