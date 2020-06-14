C$PROG PUTPUT    - Output management routine for MOC process
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE PUTPUT(ISTREAM)
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
      COMMON/LM04/ IONP,JONP(3)
      CHARACTER*4  IONP,JONP
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
      COMMON/LM16/ NRRED
      INTEGER*4    NRRED
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      INTEGER*4    ISTREAM
C
      INTEGER*4    IERR,KS,JPRM,NNBYT,KOUSIZB,ISTAT,I,J
C
      INTEGER*4    COUNT
C
      DATA         COUNT/1/
C
      INTEGER*4    LASNDX(3)
C
      INTEGER*2    LPHEDH(128)
C
      EQUIVALENCE (LPHEDH(1),LPHED(1))
C
      DATA         LASNDX/-1,-1,-1/
C
      CHARACTER*4  STAT
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR = 0
C
      IF(OUFOP.EQ.'YES ') GO TO 200
C
C     ------------------------------------------------------------------
C     Output to tape
C     ------------------------------------------------------------------
C
      KS=ISTREAM
C
   20 J=NDX(KS)
      KOUSIZB=2*OUSIZ(KS)
C
      CALL MT_WRITE(LUCO(KS),OUBUF(1,J,KS),KOUSIZB,NNBYT,IERR)
C
      IF(IERR.NE.0) THEN
      CALL IOSERR(IERR)
      MSGF='ERR '
      GO TO 30
      ENDIF
C
      JONP(KS)='YES '
      NOUT(KS)=NOUT(KS)+1
      LASNDX(KS)=NDX(KS)
      OUPTR(KS)=0
      NDX(KS)=3-NDX(KS)
      J=NDX(KS)
      OOFSET(KS)=32768*(KS-1)+16384*(J-1)
      RETURN
C
C     ------------------------------------------------------------------
C     WE HAD AN I/O ERROR: - PAUSE AND ASK FOR HELP
C     ------------------------------------------------------------------
C
   30 CALL RECLISX(NRRED)                            !LOG NRRED ETC
      IF(STAT.EQ.'EOV ') GO TO 35                    !TST FOR EOV
      IF(STAT.EQ.'EOT ') GO TO 35                    !TST FOR EOT
      GO TO 70                                       !ASK FOR HELP
C
   35 CALL TAPHAN(LUCO(KS),'BR  ',1,ISTAT)           !DO A BACKSPACE
      CALL TAPHAN(LUCO(KS),'EOF ',1,ISTAT)           !WRITE AN EOF
      CALL TAPHAN(LUCO(KS),'RW  ',1,ISTAT)           !DO A REWIND
C
      WRITE(CMSSG,40)(MTOUT(I,KS),I=1,2)
      CALL MESSLOG(LOGUT,LOGUP)
   40 FORMAT('Mount next OUTPUT-TAPE on ',2A4,'and Type: [RETURN]')
      CALL CRWAIT
C
C     ------------------------------------------------------------------
C     OUTPUT PRIMARY HEADER RECORD ON CONTINUATION TAPE
C     ------------------------------------------------------------------
C
   50 CALL MT_WRITE(LUCO(KS),LPHEDH,256,NNBYT,IERR)  !OUTPUT HEADER 
C
      IF(IERR.NE.0) CALL IOSERR(IERR)
C
      CALL IOERR(IERR,STAT)
      IF(STAT.EQ.'GOOD') GO TO 100
C
      WRITE(CMSSG,60)
      CALL MESSLOG(LOGUT,LOGUP)
   60 FORMAT('CORRECT PROBLEM AND TYPE: [RETURN]')
      CALL CRWAIT
      GO TO 50
C
C     ------------------------------------------------------------------
C     ASK FOR HELP
C     ------------------------------------------------------------------
C
   70 WRITE(CMSSG,60)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL CRWAIT
C
C     ------------------------------------------------------------------
C     OUTPUT LAST DATA RECORD TO OUTPUT TAPE (MAY BE CONTINUATION)
C     ------------------------------------------------------------------
C
  100 IF(LASNDX(KS).LT.0) GO TO 20
      J=LASNDX(KS)
      KOUSIZB=2*OUSIZ(KS)
      CALL MT_WRITE(LUCO(KS),OUBUF(1,J,KS),KOUSIZB,NNBYT,IERR)
C
      IF(IERR.NE.0) CALL IOSERR(IERR)
C
      CALL IOERR(IERR,STAT)
      IF(STAT.EQ.'GOOD') GO TO 20
      GO TO 30
C
C
C     ------------------------------------------------------------------
C     Output to LDF-file
C     ------------------------------------------------------------------
C
  200 KS=ISTREAM
      J=NDX(KS)
      KOUSIZB=2*OUSIZ(KS)
C
      CALL LDFWRIT(LUOUF,'DATA',KOUSIZB,OUBUF(1,J,KS),IERR)
C
      NOUT(KS)=NOUT(KS)+1
      LASNDX(KS)=NDX(KS)
      OUPTR(KS)=0
      NDX(KS)=3-NDX(KS)
      J=NDX(KS)
      OOFSET(KS)=32768*(KS-1)+16384*(J-1)
C
      RETURN
      END
