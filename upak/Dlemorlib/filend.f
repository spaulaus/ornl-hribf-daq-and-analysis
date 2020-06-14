C$PROG FILEND    - Writes file=marks for the MOC process
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE FILEND(IERR)
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
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF(16384,2,3)
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2    OUBUF
C     ------------------------------------------------------------------
      COMMON/LM15/ LPHED(64),NDX(3)
      INTEGER*4    LPHED,    NDX
C     ------------------------------------------------------------------
      INTEGER*4    IERR,JERR,KS,IA,IB,I,J
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      DO 50 KS=1,NOSTR
      IA=OUPTR(KS)+1                    !FIRST OUT-BUF WORD TO ZOT
      IF(IA.LE.1) GO TO 20              !TST FOR OUT-BUF EMPTY
      IB=OUSIZ(KS)                      !LAST  OUT-BUF WORD TO ZOT
      J=NDX(KS)
      DO  10 I=IA,IB                    !LOOP TO ZOT OUT-BUF
      OUBUF(I,J,KS)=-1
   10 CONTINUE
C
      CALL PUTPUT(KS)                   !DO OUTPUT
C
   20 OUPTR(KS)=0                       !RESET OUT-BUF PNTR
      OOFSET(KS)=32768*(KS-1)           !RESET OUT-BUF OFFSET
      NDX(KS)=1                         !RESET OUT-BUF NUMBER
C
      CALL FILMAR(LUCO(KS),2,1,JERR)
      IERR=IERR+JERR
   50 CONTINUE
C
      RETURN
      END
