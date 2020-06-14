C$PROG INITU     - Reads "mighty instruction list" & all chil data
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/01/98
C     ******************************************************************
C
      SUBROUTINE INITU
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC07/ MILF(262144)
      INTEGER*4    MILF
C     ------------------------------------------------------------------
      INTEGER*4    CLX,  CLY
C
      PARAMETER   (CLX=2,CLY=2048)            !MBUF DIMENSIONS
C
      COMMON/SC08/ MBUF,JBN,IPO,NBC,LX,LY,LXB,LJP
C
      INTEGER*2    MBUF(0:CLX-1,0:CLY)        !PLUS A SPARE

      INTEGER*2    JBN(0:CLY-1),IPO(0:CLY-1),NBC(CLY)

      INTEGER*4    LX,LY,LXB,LJP
C     ------------------------------------------------------------------
      COMMON/SC09/ JHPC(4096)
      INTEGER*4    JHPC
C     ------------------------------------------------------------------
      INTEGER*4    KBUF(64)
C
      CHARACTER*4  CKBUF(64)
C
      EQUIVALENCE (CKBUF,KBUF)
C
      INTEGER*2    MILH(524288)
C
      EQUIVALENCE (MILH(1),MILF(1))
C
      INTEGER*4    IREC,NPAR,NBLH,JHPREC,MILREC,GATREC,MAPREC,NWMIL
C
      INTEGER*4    IGATOF,IMAPOF,NGATL,NMAPL,NBAN,NGSET,NGXN,MILCF
C
      INTEGER*4    NDO,DBL,IA,I
C
      SAVE
C
C     ==================================================================
C     DEFINITION:  BLOCK = ONE DISK BLOCK      =  512 BYTES
C                  CLINK = ONE DISK CHAIN LINK = 1024 BYTES
C     ==================================================================
C     MAP IS A BIT MAP WHICH TELLS IF A CLINK ON DISK IS USED OR NOT
C
C     MBUF(LX,LY) = THE PARTIAL SORT BUFFER
C     LX          = THE DIMENSION (16-BIT WDS) IN EVENTS-DIRECTION
C     LXB         = THE FIRST DIMENSION OF MBUF IN BYTES
C     LY          = THE DIMENSION IN THE #-OF-BUFFERS DIRECTION
C     IPO(J)      = # OF WORDS FILLED IN JTH MBUF (I.E. MBUF( ,J))
C     NBC(J)      = THE # OF CLINKS ON DISK FOR CHAIN J
C
C     LIST - THE DOUBLE INDEXED INPUT BUFFER FOR THE EVENT-LIST
C     ILIN - THE SINGLE INDEXED INPUT BUFFER FOR THE EVENT-LIST
C     LSTL = LENGTH OF INPUT LIST IN 1/2 WORDS
C     LNBY = LENGTH OF INPUT LIST IN BYTES
C
C     NPAR = # PARMS PER EVENT
C
C     ==================================================================
C     READ IN "MIGHTY INSTRUCTION LIST" AND ALL DATA FROM CHIL
C
C     READ IN BLOCK CONTAINING SCALER VALUES
C     ==================================================================
C
      IREC=1
      CALL REDMIL(IREC,KBUF(1))
C
      NPAR  =KBUF(1)          !MAX # OF PARAMETERS PER EVENT
      MAXIP =KBUF(3)          !MAX INPUT PARAMETER
      LSTL  =KBUF(4)/2        !MAG TAPE DATA RECORD LENGTH (HALF-WDS
      NBLH  =KBUF(5)*32       !HIS-FILE SIZE IN 512-BYTE BLOCKS
      NHWH  =NBLH*256         !HIS-FILE SIZE IN 16-BIT WORDS
      JHPREC=KBUF(7)          !# RECS FOR HALF-WDS/CHAN SPEC ARRAY
      MILREC=KBUF(8)          !# RECS FOR CHIL OBJECT CODE PROPER
      GATREC=KBUF(9)          !# RECS FOR GATE-LISTS
      MAPREC=KBUF(10)         !# RECS FOR MAPPED GATE-LISTS & BANS
      LY    =KBUF(11)         !# OF PARTIAL-SORT DISK CHAINS
      IGATOF=KBUF(12)         !MIL-OFFSET FOR GATE-LISTS
      IMAPOF=KBUF(13)         !MIL-OFFSET FOR MAPS & BANS
      NGATL =KBUF(14)         !# OF GATE-LISTS
      NMAPL =KBUF(15)         !# OF MAP-LISTS
      NBAN  =KBUF(16)         !# OF BAN-LISTS
      NGSET =KBUF(17)         !# OF GATE-SETS
      NGXN  =KBUF(18)         !# OF AUX GATE-SETS
      MILCF =KBUF(19)         !LENGTH OF CHIL-PROPER IN FULL WDS
C
      IF(CKBUF(20).NE.'SCAN') GO TO 210        !TST MIL-TYPE
C
      IF(LY.GT.CLY) GO TO 210
C
      IF(NBAN.LE.0) GO TO 20                   !TST FOR BAN-SPECS
C
C     ==================================================================
C     SKIP BLOCKS ASSOCIATED WITH BAN-SPECS
C     ==================================================================
C
      NDO=4*((NBAN+127)/128)
      IREC=IREC+NDO
C
   20 IF(NGSET.LE.0) GO TO 30                  !TST FOR GATE-SETS
C
C     ==================================================================
C     SKIP BLOCKS ASSOCIATED WITH GATE-SETS
C     ==================================================================
C
      IREC=IREC+11
C
   30 IF(NGXN.LE.0) GO TO 40                   !TST FOR AUX GATE-SETS
C
C     ==================================================================
C     SKIP BLOCKS ASSOCIATED WITH AUX GATE-SETS
C     ==================================================================
C
      IREC=IREC+4
C
C     ==================================================================
C     READ #WD/CHAN MAP (JHPC), MIL, MIL GATE-REG, MIL MAP-REG
C     ==================================================================
C
   40 DO 50 I=1,JHPREC
      IA=128*(I-1)+1
      IREC=IREC+1
      CALL REDMIL(IREC,JHPC(IA))
   50 CONTINUE
C
      IA=1
      DO 60 I=1,MILREC
      IREC=IREC+1
      CALL REDMIL(IREC,MILF(IA))
      IA=IA+64
   60 CONTINUE
C
      IA=IGATOF+1
      DO 70 I=1,GATREC
      IREC=IREC+1
      CALL REDMILH(IREC,MILH(IA))
      IA=IA+128
   70 CONTINUE
C
      IA=IMAPOF+1
      DO 80 I=1,MAPREC
      IREC=IREC+1
      CALL REDMILH(IREC,MILH(IA))
      IA=IA+128
   80 CONTINUE
C
C     ==================================================================
C     INITIALIZE SOME STUFF - ONE TIME ONLY
C     ==================================================================
C
      LNBY=2*LSTL                       !TAPE DATA-RECL IN BYTES
C
      DBL=512                           !DISK BLOCK LENGTH (BYTES)
      LX=CLX                            !CLINK LENGTH IN HALF-WDS
      LXB=2*LX                          !CLINK LENGTH IN BYTES
C
C
      CLOSE(UNIT=LUC(10))               !CLOSE THE MIL-FILE
C
      NWMIL=128*MILREC                  !# WORDS ON CHIL-PROPER
C
      CALL CHILCON(NWMIL)               !CONVERT TO HALF-WD ADDRESS
C
      CALL PVSETUP(NPAR)
C
      CALL RESET
C
      RETURN
C
C     ==================================================================
C     SEND ERROR MESSAGES & EXIT
C     ==================================================================
C
  210 WRITE(CMSSG,215)
      CALL MESSLOG(LOGUT,LOGUP)
  215 FORMAT('IMPROPER .mil-TYPE FOR HISTOGRAMMING TASK')
      STOP
C
      END
