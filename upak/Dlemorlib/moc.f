C$PROG MOC       - Modify-copy processing routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE MOC(ICNF,IERR)
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
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
      COMMON/LM03/ LBUF(24576)
      INTEGER*4    LBUF
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
      COMMON/LM16/ NRRED
      INTEGER*4    NRRED
C     ------------------------------------------------------------------
      COMMON/LM17/ NPARX,RESYN
      INTEGER*4    NPARX
      CHARACTER*4        RESYN
C     ------------------------------------------------------------------
      COMMON/LM18/ MOCMO
      CHARACTER*4  MOCMO
C     ------------------------------------------------------------------
      COMMON/LM19/ JCNF,IREBF
      CHARACTER*4  JCNF,IREBF
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      CHARACTER*4  ICNF,STAT,KEND,KMD
C
      INTEGER*4    IERR
C
      INTEGER*4    KS,NFTOP,NBTOP,NCEOF,NFRED,IV,JERR
C
      INTEGER*4    JFIR,JLAS,NDO
C
      INTEGER*2    LIST(16384,3),LLIST(49152)
C
      EQUIVALENCE (LIST(1,1),LBUF),(LLIST,LBUF)
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      SAVE
C
C     ------------------------------------------------------------------
C     MOC & MOCE CMDS   - PROCESSED HERE
C
C     TERMINATION BY---   RESULTS IN ----------------------------
C     REQUESTED # FILES   FLUSH OUBUF, 2-EOF, 1-BKFIL, ICNF='NO  '
C     EOM ON INPUT        FLUSH OUBUF, 2-EOF, 1-BKFIL, ICNF='NO  '
C     REQUESTED # RECS    FLUSH OUBUF, 2-EOF, 2-BKFIL, ICNF='YES '
C     SEND STOP                        2-EOF, 2-BKFIL, ICNF='YES '
C     INPUT ERROR         FLUSH OUBUF, 2-EOF, 2-BKFIL, ICNF='NO  '
C     MOCE                FLUSH OUBUF, 2-EOF, UNLOAD TAPES, EXIT
C     ------------------------------------------------------------------
C
      IERR=0
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0)          GO TO 500
C
      IF(MOCMO.EQ.'UPON')    GO TO 5    !TST FOR 'UPON'
      IF(LMLO.EQ.'NO  ')     GO TO 520  !TST FOR MIL-FILE OPEN
C
    5 IF(INFOP.NE.'YES '.AND.
     &   LINO .EQ.'NO  ')    GO TO 540  !TST FOR INPUT    OPEN
C
      IF(OUFOP.NE.'YES ') THEN
      DO 10 KS=1,NOSTR
      IF(LOUO(KS).EQ.'NO  ') GO TO 560  !TST FOR OUTPUT   OPEN
   10 CONTINUE
      ENDIF
C
      KMDS=KMD                          !SAVE COMMAND
      KEND='NO  '
      IF(KMD.EQ.'MOCE') KEND='YES '     !TST FOR END ON REQ COMPLETE
C
      NFTOP=1                           !RESET # FILES TO PROCESS
      NBTOP=100000000                   !SET # BUFFERS TO PROCESS
      NCEOF=0                           !RESET DBL-EOF CNTR
      NFRED=0                           !RESET # FILES READ
      NRRED=0                           !RESET # BUFFS READ
C
      DO 20 KS=1,NOSTR
      NOUT(KS)=0                        !RESET # BUFFS OUTPUT
   20 CONTINUE
C
      IF(NF.LT.2)   GO TO 100           !TST FOR "NFTOP" SPECIFIED
      CALL IVALU(LWD(1,2),IV,JERR)      !PICK UP "NFTOP"
      IF(JERR.NE.0) GO TO 500           !TST FOR SYNTAX ERROR
      NFTOP=IV                          !SET "NFTOP"
      IF(NF.LT.3)   GO TO 100           !TST FOR # BUFFERS FIELD
      CALL IVALU(LWD(1,3),IV,JERR)
      IF(JERR.NE.0) GO TO 500
      NBTOP=IV
C
C     ------------------------------------------------------------------
C     PROCESSING LOOP STARTS HERE
C     ------------------------------------------------------------------
C
  100 IF(ICNF.EQ.'NO  ') GO TO 110      !ARE WE TO "CONTINUE"
      ICNF='NO  '                       !RESET CONTINUE-FLAG
      GO TO 120                         !AND CONTINUE
C
  110 CALL INPUT(LIN,1,JFIR,JLAS,STAT)  !OTHERWISE, DO FIRST INPUT
C
      IF(MOCMO.EQ.'UPON') CALL UPRSCRUB
      IF(MOCMO.NE.'UPON') CALL  PRSCRUB
      JCNF='NO  '
      GO TO 130
C
  120 CALL INPUT(LIN,2,JFIR,JLAS,STAT)  !DO SUCCESSIVE INPUTS
C
  130 IF(STAT.EQ.'GOOD') GO TO 200      !TST FOR ERROR
      IF(MSGF.NE.'    ') GO TO 330      !TST STOP FLAG
      IF(STAT.EQ.'EOF ') GO TO 140      !TST FOR EOF ENCOUNTERED
C
      IF(STAT.EQ.'EOT ') GO TO 360      !TST FOR EOT
      IF(STAT.EQ.'EOV ') GO TO 360      !TST FOR EOV
C
      NCEOF=0                           !RESET DBL-EOF CNTR
      IF(STAT.EQ.'PE  ') GO TO 150      !TST FOR PARITY ERROR
      GO TO 340                         !GO ASK FOR INSTRUCTIONS
C
  140 NFRED=NFRED+1                     !BUMP # FILES READ CNTR
      NCEOF=NCEOF+1                     !BUMP DBL-EOF CNTR
      CALL MT_CSE(LUCI,JERR)
      IF(JERR.NE.0)GOTO 340
      IF(NCEOF.GE.2)     GO TO 350      !TST FOR DBL-EOF
C
      WRITE(CMSSG,145)NFRED
      CALL MESSLOG(LOGUT,LOGUP)
  145 FORMAT(I6,' FILES PROCESSED....')
C
      CALL FILEND(IERR)                 !CLOSE FILE
      CALL RECLIS(NRRED)                !DISLPAY # RECS I/OED
      IF(IERR.NE.0)      GO TO 370      !TST FOR ERROR
      IF(NFRED.EQ.NFTOP) GO TO 310      !TST FOR REQUIRED # FILES
      GO TO 110                         !OTHERWISE, KEEP ON TRUCKIN
C
  150 CALL TAPHAN(LUCI,'FR  ',1,STAT)   !DO ONE FORWARD-RECORD
      WRITE(CMSSG,160)
      CALL MESSLOG(LOGUT,LOGUP)
  160 FORMAT('ONE RECORD SKIPPED')
      IF(STAT.EQ.'GOOD') GO TO 110
      IF(STAT.EQ.'EOF ') GO TO 140      !TST FOR EOF ENCOUNTERED
      GO TO 340                         !OTHERWISE, ASK FOR HELP
C
  200 NCEOF=0                           !RESET DBL-EOF CNTR
C
      NDO=JLAS-JFIR+1
      IF(MOCMO.EQ.'UPON') CALL USERMOC(LLIST(JFIR),NDO)
      IF(MOCMO.NE.'UPON') CALL  CHILUM(LLIST(JFIR),NDO)
C
      NRRED=NRRED+1                     !INC RECORD COUNTER
      IF(NRRED.GE.NBTOP) GO TO 320      !TST REQUESTED # RECS
      IF(MSGF.NE.'    ') GO TO 330      !TST FOR STOP FLAG
      GO TO 120                         !OTHERWISE, KEEP ON TRUCKIN
C
C     ------------------------------------------------------------------
C     PROCESSING LOOP ENDS HERE
C     ------------------------------------------------------------------
C
C
C     ********************************** REQUESTED # FILES PROCESSED
C
  310 IF(KEND.EQ.'YES ') CALL QUITTER
      ICNF='NO  '
      GO TO 400
C
C     ********************************** REQUESTED # BUFFS PROCESSED
C
  320 CALL FILEND(IERR)
      CALL FILMARM(0,1,IERR)
      CALL RECLIS(NRRED)
      IF(KEND.EQ.'YES ') CALL QUITTER
C
      IF(MOCMO.EQ.'UPON') CALL UPRSCRUB
      IF(MOCMO.NE.'UPON') CALL  PRSCRUB
C
      ICNF='YES '
      GO TO 400
C
C     ********************************** SEND STOP REQUEST .........
C
  330 MSGF='    '
      CALL FILMARM(2,2,IERR)
      CALL RECLIS(NRRED)
      ICNF='YES '
      IERR=1
      RETURN
C
C     ********************************** I/O ERROR OF SOME SORT ....
C
  340 CALL FILEND(IERR)
      CALL FILMARM(0,1,IERR)
      CALL RECLIS(NRRED)
      ICNF='NO  '
      IERR=1
      RETURN
C
C     ********************************** DOUBLE EOF ENCOUNTERED ....
C
  350 CALL FILMARM(1,1,IERR)
      WRITE(CMSSG,355)
      CALL MESSLOG(LOGUT,LOGUP)
  355 FORMAT('DOUBLE END-OF-FILE ENCOUNTERED')
      IF(KEND.EQ.'YES ') CALL QUITTER
      ICNF='NO  '
      GO TO 400
C
C     ********************************** END-OF-MEDIUM ENCOUNTERED
C
  360 IF(NCEOF.GT.0) GO TO 365
      CALL FILEND(IERR)
      CALL RECLIS(NRRED)
  365 IF(KEND.EQ.'YES ') CALL QUITTER
      ICNF='NO  '
      GO TO 400
C
C     ********************************** GO ASK FOR HELP ...........
C
  370 IERR=1
      RETURN
C
C     ------------------------------------------------------------------
C     NORMAL RETURN
C     ------------------------------------------------------------------
C
  400 RETURN
C
C     ------------------------------------------------------------------
C     DISPLAY AND LIST ERROR MESSAGES & TAKE ERROR RETURN
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,510)
      CALL MESSLOG(LOGUT,LOGUP)
  510 FORMAT('ILLEGAL VALUE OR SYNTAX ERROR - CMD IGNORED')
      IERR=1
      RETURN
C
  520 WRITE(CMSSG,530)
      CALL MESSLOG(LOGUT,LOGUP)
  530 FORMAT('MIL-FILE NOT OPEN & "UPOF MODE" - COMMAND IGNORED')
      IERR=1
      RETURN
C
  540 WRITE(CMSSG,550)
      CALL MESSLOG(LOGUT,LOGUP)
  550 FORMAT('INPUT TAPE/FILE NOT ASSIGNED - COMMAND IGNORED')
      IERR=1
      RETURN
C
  560 WRITE(CMSSG,570)KS
      CALL MESSLOG(LOGUT,LOGUP)
  570 FORMAT('TAPE NOT ASSIGNED FOR OUTPUT-',I1,' - CMD IGNORED')
      IERR=1
      RETURN
C
      END
