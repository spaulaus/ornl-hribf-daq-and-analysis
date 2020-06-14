C$PROG TAPOPEN   - Opens & closes tape units for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/17/99
C     ******************************************************************
C
      SUBROUTINE TAPOPEN(IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAR
C     ------------------------------------------------------------------
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC22/ IPCNAM(20),LDFNAM(20),TAPNAM(20),BANNAM(20)
      INTEGER*4    IPCNAM,    LDFNAM,    TAPNAM,    BANNAM
C     ------------------------------------------------------------------
      CHARACTER*80 CTAPNAM
      EQUIVALENCE (CTAPNAM,TAPNAM)
C
      INTEGER*2 MLSTH(26)
      INTEGER*4 MLSTF(13),CHAN
      CHARACTER*80 TAPE
      CHARACTER*8 CLWD
      INTEGER*4 ITAPE(20)
      DATA CHAN/-1/
C
      CHARACTER*4  KMD,KMX
C
      EQUIVALENCE (TAPE,ITAPE)
      EQUIVALENCE (MLSTF(1),MLSTH(1))
      EQUIVALENCE (LWD(1,1),CLWD)
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(1,2))
C
      DATA ITAPE/20*0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0                            !RESET ERROR FLAG
C
      CTAPNAM='Undefined'
C
      TAPE=' '
C
      IF(KMD.EQ.'CLOT') GO TO 100       !TST FOR DISMOUNT REQUEST
      IF(KMD.EQ.'CLUN') GO TO 100       !TST FOR DISMOUNT & UNLOAD
C
      IF(LUC(1).GT.0)   GO TO 230       !TST FOR ALREADY ASSIGNED
C
      CALL MKTAPNAM(LWD(1,2),ITAPE)
C
      IF(TAPE.EQ.' ') GO TO 100
C
      CALL MT_OPENRO(ITAPE,CHAN)      	!OPEN TAPE
C
      IF(CHAN.LT.0) GO TO 100
C
      CTAPNAM=CLWD                      !Save tape name for STATMAN
C
      LUC(1)=CHAN                       !SAVE CHAN IN LUC(1)
      LUC(2)=CHAN                       !SAVE CHAN IN LUC(2)
      RETURN
C
  100 IF(LUC(1).LT.0) GO TO 240         !TST FOR NOT ASSIGNED
C
      IF(KMD.EQ.'CLUN') THEN
      CALL MT_REWUL(LUC(1),IERR)
      IF(IERR.NE.0) GO TO 220
                        ENDIF
C
      CALL MT_CLOSE(LUC(1))
C 
      LUC(1)=-1                         !SET IT CLOSED
      LUC(2)=-1                         !SET IT CLOSED
      RETURN
C
C$ENTR TAPCLEAR  - Tries to clear tape status "exceptions"
C
      ENTRY TAPCLEAR(IERR)
C
C
C     **************************************************************
C     THIS CODE TRIES TO COVER FOR GAPS IN ULTRIX SCSI TAPE DRIVER
C     BY DOING A CLOSE AND REOPEN OF TAPE. THIS APPEARS TO BE NEEDED 
C     CLEAR CERTAIN ERROR CONDITIONS. PROBABLY JUST OPEN IS ENOUGH.
C     **************************************************************
C
      IERR=0                            !RESET ERROR FLAG
      IF(IERR.EQ.0) RETURN              !Do nothing
C
      IF(CHAN.LT.0.OR.LENGTH.EQ.0)THEN
         WRITE(CMSSG,265)
         IERR=990
         CALL MESSLOG(6,7)
         RETURN 
      ENDIF
c
      CALL MT_CLOSE(LUC(1))
C
C     write(6,'('' Tape autoclr/continue on chan:'',i6,'' tape:'',A20)')
C    1    chan,tape
C
      CALL MT_OPENRO(ITAPE,CHAN)      	!OPEN TAPE
      IF(CHAN.LT.0) THEN
         LUC(1)=-1
         LUC(2)=-1
         IERR=990
         WRITE(CMSSG,265)
         CALL MESSLOG(6,7)
         RETURN 
      ENDIF 

      LUC(1)=CHAN                       !SAVE CHAN IN LUC(1)
      LUC(2)=CHAN                       !SAVE CHAN IN LUC(2)
      RETURN
C
C     ------------------------------------------------------------------
C     DISPLAY AND LOG ERROR MESSAGES - SET ERROR FLAG
C     ------------------------------------------------------------------
C
  220 WRITE(CMSSG,225)STAT
  225 FORMAT('ERROR REWINDING TAPE - STATUS =',I6)
      GO TO 300
C
  230 WRITE(CMSSG,235)
  235 FORMAT('TAPE ALREADY ASSIGNED - REQUEST IGNORED')
      GO TO 300
C
  240 WRITE(CMSSG,245)
  245 FORMAT('TAPE NOT ASSIGNED - REQUEST IGNORED')
      GO TO 300
C
  250 WRITE(CMSSG,255)
  255 FORMAT('DEVICENAME NOT BUILT - REQUEST IGNORED')
C
  265 FORMAT('AUTO-RECOVER FROM TAPE READ ERROR FAILED')
  300 CALL MESSLOG(6,7)
      IERR=1
      RETURN
      END
