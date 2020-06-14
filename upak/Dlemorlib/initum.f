C$PROG INITUM    - Reads in MIL-file & initializes CHIL parameters
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE INITUM
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
      COMMON/LM02/ MBUF(4,5),JBN(4),IPO(4),NBC(4),LX,LY,LXB,LJP
      INTEGER*2    MBUF,     JBN,   IPO,   NBC
      INTEGER*4                                   LX,LY,LXB,LJP
C     ------------------------------------------------------------------
      COMMON/LM03/ LBUF(24576)
      INTEGER*4    LBUF
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM12/ MILF(262144)
      INTEGER*4    MILF
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
      INTEGER*4    IREC,JHPSEC,MILSEC,MAPSEC,IGATOF,IMAPOF,NBAN,NGSET
C
      INTEGER*4    NGXN,NDO,NN,IA,NWMIL,I
C
      INTEGER*4    KBUF(64),GATSEC
C
      CHARACTER*4  CBUF(64)
C
      EQUIVALENCE (CBUF,KBUF)
C
      SAVE
C
C     ------------------------------------------------------------------
C     LSTL = LENGTH OF INPUT LIST IN 1/2 WORDS
C     LNBY = LENGTH OF INPUT LIST IN BYTES
C     NPAR = # PARMS PER EVENT
C
C     ------------------------------------------------------------------
C     READ IN BLOCK CONTAINING SCALER VALUES
C     ------------------------------------------------------------------
C
      IREC=1
      CALL REDMIL(IREC,KBUF(1))
C
      NPAR  =KBUF(1)
      LNBY  =KBUF(4)
      LSTL  =LNBY/2
      JHPSEC=KBUF(7)
      MILSEC=KBUF(8)
      GATSEC=KBUF(9)
      MAPSEC=KBUF(10)
      LX=1
      LY=KBUF(11)
      IGATOF=KBUF(12)
      IMAPOF=KBUF(13)
      NBAN  =KBUF(16)
      NGSET =KBUF(17)
      NGXN  =KBUF(18)
C
      NOSTR=0
      DO 10 I=1,3
      IF(CBUF(I+20).NE.'NO  ') NOSTR=I
   10 CONTINUE
C
      IF(CBUF(20).NE.'PRES') GO TO 210         !TST MIL-TYPE
      IF(MILSEC.GT.256)      GO TO 220         !TST MIL-SIZE
C
C
      IF(NBAN.LE.0) GO TO 20                   !TST FOR BAN-SPECS
C
C     ------------------------------------------------------------------
C     SKIP BLOCKS ASSOCIATED WITH BAN-SPECS
C     ------------------------------------------------------------------
C
      NDO=4*((NBAN+127)/128)
      DO 15 NN=1,NDO
      IREC=IREC+1
      CALL REDMIL(IREC,KBUF(1))
   15 CONTINUE
C
   20 IF(NGSET.LE.0) GO TO 30                  !TST FOR GATE-SETS
C
C     ------------------------------------------------------------------
C     SKIP BLOCKS ASSOCIATED WITH GATE-SETS
C     ------------------------------------------------------------------
C
      DO 25 NN=1,11
      IREC=IREC+1
      CALL REDMIL(IREC,KBUF(1))
   25 CONTINUE
C
   30 IF(NGXN.LE.0) GO TO 40                   !TST FOR AUX GATE-SETS
C
C     ------------------------------------------------------------------
C     SKIP BLOCKS ASSOCIATED WITH AUX GATE-SETS
C     ------------------------------------------------------------------
C
      DO 35 NN=1,4
      IREC=IREC+1
      CALL REDMIL(IREC,KBUF(1))
   35 CONTINUE
C
C     ------------------------------------------------------------------
C     READ #WD/CHAN MAP (JHPC), MIL, MIL GATE-REG, MIL MAP-REG
C     ------------------------------------------------------------------
C
   40 DO 50 I=1,JHPSEC
      IA=128*(I-1)+1
      IREC=IREC+1
      CALL REDMIL(IREC,KBUF(1))
   50 CONTINUE
C
      IA=1
      DO 60 I=1,MILSEC
      IREC=IREC+1
      CALL REDMIL(IREC,MILF(IA))
      IA=IA+64
   60 CONTINUE
C
      IA=IGATOF/2+1
      DO 70 I=1,GATSEC
      IREC=IREC+1
      CALL REDMIL(IREC,MILF(IA))
      IA=IA+64
   70 CONTINUE
C
      IA=IMAPOF/2+1
      DO 80 I=1,MAPSEC
      IREC=IREC+1
      CALL REDMIL(IREC,MILF(IA))
      IA=IA+64
   80 CONTINUE
C
      CLOSE(UNIT=LML)
C
      NWMIL=128*MILSEC
C
      CALL CHILCON(NWMIL)
C
      RETURN
C
C     ------------------------------------------------------------------
C     Report any errors & exit
C     ------------------------------------------------------------------
C
  210 WRITE(CMSSG,215)
      CALL MESSLOG(LOGUT,LOGUP)
  215 FORMAT('IMPROPER MIL-FILE TYPE FOR PRE-SCAN TASK')
      CALL EXITOR(1)
C
  220 WRITE(CMSSG,225)
      CALL MESSLOG(LOGUT,LOGUP)
  225 FORMAT('MIL-FILE TOO LONG FOR PRE-SCAN TASK')
      CALL EXITOR(1)
C
      END
