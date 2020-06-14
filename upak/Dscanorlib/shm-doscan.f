C$PROG DOSCAN    - Executes the scan process for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/29/99
C     ******************************************************************
C
      SUBROUTINE DOSCAN(RETN)
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
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC04/ JCNF,IHEDN,MBFL
      INTEGER*4         IHEDN,MBFL
      CHARACTER*4  JCNF
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC06/ LIST(16384,2)
      INTEGER*2    LIST
C     ------------------------------------------------------------------
      COMMON/SC12/ MEM_STYLE,SHMID
      CHARACTER*80 MEM_STYLE
      INTEGER*4    SHMID
C     ------------------------------------------------------------------
      COMMON/SC14/ NBRED,NBTOP,ICNF
      INTEGER*4    NBRED,NBTOP
      CHARACTER*4              ICNF
C     ------------------------------------------------------------------
      COMMON/SC15/ NCEOF,LAUTO
      INTEGER*4    NCEOF
      CHARACTER*4        LAUTO
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      COMMON/SC20/ SCANTYP
      CHARACTER*4  SCANTYP
C     ------------------------------------------------------------------
      COMMON/SC25/ CNAMS                   !CNAMS contains SHM filename
      CHARACTER*80 CNAMS
C     ------------------------------------------------------------------
      COMMON/ORPHAS/ STRBUFEVT,NUMBUFEVTS,BUF_NUM,LASTEVT,SUMEVTS,
     &               BEGINEVT
C
      REAL*8         STRBUFEVT,NUMBUFEVTS,BUF_NUM,LASTEVT,SUMEVTS,
     &               BEGINEVT
C     ------------------------------------------------------------------
      CHARACTER*4  KOM
C
      EQUIVALENCE (KOM,LWD(1,1))
C
      INTEGER*4    NFTOP,NFRED,JERR,IERR,IERRR,IV,RETN,STAT
C
      INTEGER*4    JFIR,JLAS,JMAX,TSTAT
C
      INTEGER*2    LLIST(32768)
C
      EQUIVALENCE (LLIST,LIST)
C
      INTEGER*4    LUT,LUH,LUD
C
      EQUIVALENCE (LUT,LUC(1)),(LUH,LUC(6)),(LUD,LUC(9))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      RETN=0
C
      IF(KOM.EQ.'GO  ') GO TO 250
      IF(KOM.EQ.'GOEN') GO TO 250
C
      RETURN
C   
C     ==================================================================
C     READ AND PROCESS RECORDS UNTIL YOU HIT AN ABNORMAL CONDITION
C     ==================================================================
C   
  250 CONTINUE
C
      IF(INTYP.EQ.'    ') GO TO 1020    !Tst for input stream defined
C
C     ------------------------------------------------------------------
C     For TAPE, LNBY is set by RECL command (default is 32768).  For SHM
C     the value for LNBY is returned by the call to open the SHM stream.
C     ------------------------------------------------------------------
C
      SUMEVTS=0.0                       !Reset event counter
      LASTEVT=0.0                       !Clear last  event number
      BEGINEVT=0.0                      !Clear first event number
C
      NFTOP=0                           !RESET # FILES TO PROCESS
      NBTOP=100000000                   !SET # BUFFERS TO PROCESS
      NCEOF=0                           !RESET DBL-EOF CNTR
      NFRED=0                           !RESET # FILES READ
      IF(NF.LT.2)   GO TO 300           !TST FOR "NFTOP" SPECIFIED
C   
      CALL IVALU(LWD(1,2),IV,JERR)      !PICK UP "NFTOP"
      IF(JERR.NE.0) GO TO 1010          !TST FOR SYNTAX ERROR
C   
      NFTOP=IV                          !SET "NFTOP"
      IF(NF.LT.3)   GO TO 300           !TST FOR # BUFFERS FIELD
C   
      CALL IVALU(LWD(1,3),IV,JERR)
      IF(JERR.NE.0) GO TO 1010
C   
      NBTOP=IV
C   
C     ================================== PROCESS LOOP STARTS HERE !*
C     ================================== PROCESS LOOP STARTS HERE !*
C                                                                 !*
  300 IF(INTYP.EQ.'SHM ') GO TO 310     !Tst for SHM input        !*
C                                                                 !*
      IF(ICNF.EQ.'NO  ')  GO TO 305     !Tst for continue mode    !*
      ICNF='NO  '                       !Reset continue-flag      !*
      GO TO 330                         !and continue             !*
C                                                                 !*
  305 CALL INPUT(1,JFIR,JLAS,IERR)      !Do 1st input tape or LDF !*
      JCNF='NO  '                                                 !*
      GO TO 340                                                   !*
C                                                                 !*
  310 CALL INPUTIPC(IERR)               !Do 1st input from SHM    !*
      IF(IERR.EQ.0) THEN                                          !*
      BEGINEVT=STRBUFEVT                                          !*
      JFIR=1                                                      !*
      JLAS=LNBY/2                                                 !*
      WRITE(CMSSG,315)LNBY                                        !*
  315 FORMAT('Input record size is ',I6,' bytes')                 !*
      CALL MESSLOG(LOGUT,LOGUP)                                   !*
      ENDIF                                                       !*
      GO TO 340                                                   !*
C                                                                 !*
C     ============================================================!*
C     Successive input starts at statement 320                    !*
C     ============================================================!*
C                                                                 !*
C                                       !Do successive input from !*
  320 IF(INTYP.EQ.'SHM ') THEN          !SHM ?                    !*
      CALL INPUTIPC(IERR)                                         !*
      JFIR=1                                                      !*
      JLAS=LNBY/2                                                 !*
      GO TO 340                                                   !*
      ENDIF                                                       !*
C                                       !Do successive input from !*
  330 CALL INPUT(2,JFIR,JLAS,IERR)      !tape or LDF              !*
C                                                                 !*
C     ============================================================!*
C                                                                 !*
  340 IF(IERR.EQ.0)      THEN                                     !*
      CALL VMETCHEK(LLIST,TSTAT)        !Check VME Clock          !*
      IF(TSTAT.EQ.-1)    GO TO 320      !Below lo-limit??         !*
      IF(TSTAT.EQ.1)     RETURN         !Above hi-limit??         !*
      GO TO 450                                                   !*
      ENDIF                                                       !*
C                                                                 !*
      IF(IERR.EQ.999)    GO TO 350      !TST FOR EOF ENCOUNTERED  !*
      IF(MSGF.NE.'    ') RETURN         !Tst for Ctrl/C           !*
C                                                                 !*
      CALL VMETCHEK(LLIST,TSTAT)        !Check VME Clock          !*
      IF(TSTAT.EQ.-1) GO TO 320         !Below lo-limit??         !*
      IF(TSTAT.EQ.1)  RETURN            !Above hi-limit??         !*
C                                                                 !*
      IF(NFTOP.EQ.0)     GO TO 460      !KEEP-ON-TRUCKIN MODE?    !*
      NCEOF=0                           !RESET DBL-EOF CNTR       !*
      IF(IERR.EQ.5)      GO TO 350      !UNIX HARDWARE IO ERROR   !*
C                                                                 !*
      GO TO 460                         !GO ASK FOR INSTRUCTIONS  !*
C                                                                 !*
  350 NFRED=NFRED+1                     !BUMP # FILES READ CNTR   !*
      NCEOF=NCEOF+1                     !BUMP DBL-EOF CNTR        !*
      IF(INTYP.EQ.'TAPE') THEN          !Tst for tape input       !*
      CALL TAPCLEAR(IERRR)              !CLEAR FUNNY ULTRIX ERROR !*
      ENDIF                             !                         !*
C                                                                 !*
      IF(MSGF.NE.'    ') RETURN         !Tst for Ctrl/C           !*
C                                                                 !*
      IF(NCEOF.GE.2)     GO TO 352      !TST FOR DBL-EOF          !*
      IF(NFRED.EQ.NFTOP) GO TO 460      !TST FOR REQUIRED # FILES !*
      GO TO 300                         !OTHERWISE, KEEP TRUCKIN  !*
C                                                                 !*
  352 WRITE(CMSSG,354)                                            !*
      CALL MESSLOG(LOGUT,LOGUP)                                   !*
  354 FORMAT('DOUBLE END-OF-FILE ENCOUNTERED')                    !*
      GO TO 460                                                   !*
C                                                                 !*
  450 NCEOF=0                           !RESET DBL-EOF CNTR       !*
C                                                                 !*
C                                                                 !*
      IF(SCANTYP.EQ.'USER') THEN        !Tst for SCANTYP=USER     !*
      CALL HISSUB(LLIST(JFIR),JLAS-JFIR+1)                        !*
      GO TO 455                                                   !*
      ENDIF                                                       !*
C                                                                 !*
      IF(SCANTYP.EQ.'CHIL') THEN        !Tst for SCANTYP=CHIL     !*
      JMAX=JLAS-JFIR+1                                            !*
      CALL CHILUM(LLIST(JFIR),JMAX)                               !*
      ENDIF                                                       !*
C                                                                 !*
  455 NBRED=NBRED+1                     !INC # BUFS READ          !*
C                                                                 !*
      IF(NBRED.GE.NBTOP) THEN           !TST FOR DONE             !*
                         ICNF='YES '    !IF YES, SET CONT-FLAG    !*
                         GO TO 465      !GO SEE WHAT TO DO NEXT   !*
                         ENDIF                                    !*
C                                                                 !*
      IF(MSGF.NE.'    ') RETURN         !TST for Ctrl/C           !*
C                                                                 !*
      GO TO 320                         !OTHERWISE, KEEP TRUCKIN  !*
C                                                                 !*
C     ================================== PROCESS LOOP ENDS HERE   !*
C     ================================== PROCESS LOOP ENDS HERE   !*
C
C     ------------------------------------------------------------------
C     TEST FOR  - SIMPLE GO, AUTO END, OR TAPE-WAIT MODE
C     ------------------------------------------------------------------
C   
  460 ICNF='NO  '                       !RESET CONT-FLAG
  465 WRITE(CMSSG,470)NBRED             !DISP/LIST # BUFFS PROCESSED
C
  470 FORMAT('Number of buffers processed =',I8)
C
C
      IF(INTYP.EQ.'SHM ') THEN          !If SHM, report
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,475)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,480)LASTEVT-BEGINEVT, !#events sent
     &        SUMEVTS,                  !#events seen
     &        LASTEVT-BEGINEVT-SUMEVTS  !#events lost
      CALL MESSLOG(LOGUT,LOGUP)
      ICNF='NO  '
      ENDIF
C
  475 FORMAT('  Total Events   Seen Events   Lost Events')
  480 FORMAT(3F14.0)
C
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(INTYP.EQ.'TAPE') THEN          !Tst for tape input
      CALL TAPCLEAR(IERRR)              !CLEAR EOF FLAG "JUST IN CASE"
      ENDIF
C   
      IF(KOM.EQ.'GO  ') RETURN          !TST FOR SIMPLE GO
C                                       !OTHERWISE, GOEN
C
      IF(LUT.GE.0) THEN                 !TEST FOR TAPE ASSIGNED
      KOM='CLUN'                        !DISMOUNT & UNLOAD TAPE
      CALL TAPOPEN(IERR)
      ENDIF
C
      IF(INTYP.EQ.'SHM ') THEN          !Test  for SHM assigned
      CALL CLOSEIPC()                   !Detach from SHM and
      INTYP='    '                      !remove blocked semaphores
      ENDIF
C
C   
C     ------------------------------------------------------------------
C     WINDUP - WINDUP - WINDUP - WINDUP - WINDUP - WINDUP - WINDUP
C     ------------------------------------------------------------------
C   
      CALL HISNIT(LUH,'HUP ')
      IF(KOM.EQ.'HUP ') RETURN
C
      OPEN(UNIT       = 21,
     &     FILE       = CNAMS,
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = STAT)
C
      IF((MEM_STYLE(1:5).NE.'LOCAL')) THEN  !Test for & delete 
      CALL SHM_DELETE(SHMID, IERR)          !shared memory segment
      CLOSE(UNIT=21,STATUS='DELETE') 
      ENDIF
C
      STOP
C
C     ------------------------------------------------------------------
C     LIST ERROR MESSAGES
C     ------------------------------------------------------------------
C
 1010 WRITE(CMSSG,1015)
      CALL MESSLOG(LOGUT,LOGUP)
 1015 FORMAT('SYNTAX ERROR OR ILLEGAL COMMAND - IGNORED')
      RETN=50
      RETURN
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Please define an input data stream (TAPE, SHM or LDF)')
      CALL MESSLOG(LOGUT,0)
      RETURN
C
      END
