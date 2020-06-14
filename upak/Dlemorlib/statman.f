C$PROG STATMAN   - Displays/logs Tape & File status information
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE STATMAN
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
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM09/ KINPUT,INRECN
      CHARACTER*4  KINPUT
      INTEGER*4           INRECN
C     ------------------------------------------------------------------
      COMMON/LM13/ NUMINT,NUMOUT(3),NOUT(3),MTIN(6),MTOUT(6,3),NOSTR
      INTEGER*4    NUMINT,NUMOUT,   NOUT,   MTIN,   MTOUT,     NOSTR
C     ------------------------------------------------------------------
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF(16384,2,3)
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2    OUBUF
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
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      COMMON/LM24/ LAUTO,INRECL
      CHARACTER*4  LAUTO
      INTEGER*4          INRECL
C     ------------------------------------------------------------------
      COMMON/LM26/ LDFONAM,LDFINAM
      CHARACTER*80 LDFONAM,LDFINAM
C     ------------------------------------------------------------------
      COMMON/LM27/ EVLNAM
      CHARACTER*80 EVLNAM
C     ------------------------------------------------------------------
      COMMON/LM29/ FMTI,FMTO
      CHARACTER*4  FMTI,FMTO
C     ------------------------------------------------------------------
      COMMON/LM30/ GATNAM(20),GATLO(1000),GATHI(1000),GATMX,GATOP,LUGAT
      INTEGER*4    GATNAM,    GATLO,      GATHI,      GATMX,      LUGAT
      CHARACTER*4                                           GATOP
C     ------------------------------------------------------------------
      COMMON/LM31/ SWAPLDF
      CHARACTER*4  SWAPLDF
C     ------------------------------------------------------------------
      COMMON/LM32/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
C     ------------------------------------------------------------------
      CHARACTER*4  KMX
C
      INTEGER*4    INTYPI,OUTYPI
      EQUIVALENCE (INTYPI,INTYP),(OUTYPI,OUTYP)
C
      INTEGER*4    I,LEN,STRLEN
C
      CHARACTER*12 FLAG
C
      EQUIVALENCE (KMX,LWD(1,2))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KMX.EQ.'GATE')   GO TO 1500
C
      IF(INTYP.EQ.'    ')  GO TO 500
      IF(INTYPI.EQ.0)      GO TO 500
C
      WRITE(CMSSG,50)
   50 FORMAT(8('=========='))
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(INTYP.EQ.'TAPE') GO TO 100
      IF(INTYP.EQ.'LDF ') GO TO 200
      IF(INTYP.EQ.'EVEL') GO TO 300
      GO TO 500
C
  100 WRITE(CMSSG,105)MTIN
  105 FORMAT('Input is assigned to: ------------------ ',6A4)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 500
C
  200 LEN=STRLEN(LDFINAM)
      IF(LEN.GT.70) LEN=70
      WRITE(CMSSG,205)LDFINAM(1:LEN)
  205 FORMAT('Input  assigned to: -------------------- ',A)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(FLAG,810)INDIR(2)
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,210)FLAG
  210 FORMAT('Maximum input  record  = --------------- ',A)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(FLAG,810)INRECI
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,220)FLAG
  220 FORMAT('Current input  pointer = --------------- ',A)
      CALL MESSLOG(LOGUT,LOGUP)
      IF(SWAPLDF.NE.'YES ') GO TO 500
      WRITE(CMSSG,225)
  225 FORMAT('Automatic byte-swap of input LDF ------- Enabled')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 500
C
  300 LEN=STRLEN(EVLNAM)
      IF(LEN.GT.70) LEN=70
      WRITE(CMSSG,205)EVLNAM(1:LEN)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(FLAG,810)INRECN
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,310)FLAG
  310 FORMAT('Current input  pointer = --------------- ',A)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 500
C
  500 IF(OUTYP.EQ.'    ')  GO TO 800
      IF(OUTYPI.EQ.0)      GO TO 800
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
C
      IF(OUTYP.EQ.'TAPE') GO TO 600
      IF(OUTYP.EQ.'LDF ') GO TO 700
      GO TO 800
C
  600 WRITE(CMSSG,605)(MTOUT(I,1),I=1,6)
  605 FORMAT('Output assigned to: -------------------- ',6A4)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 800
C
  700 LEN=STRLEN(LDFONAM)
      IF(LEN.GT.70) LEN=70
      WRITE(CMSSG,705)LDFONAM(1:LEN)
  705 FORMAT('Output assigned to: -------------------- ',A)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(FLAG,810)OUDIR(2)
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,710)FLAG
  710 FORMAT('Maximum output record  = --------------- ',A)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(FLAG,810)OURECI
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,720)FLAG
  720 FORMAT('Current output pointer = --------------- ',A)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 800
C
  800 WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1000)LFORM
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1005)FMTO
      CALL MESSLOG(LOGUT,LOGUP)
C
  810 FORMAT(I12)
C
      WRITE(FLAG,810)LNBY
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,1010)FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      FLAG='Disabled'
      IF(LAUTO.EQ.'YES ') FLAG='Enabled'
      WRITE(CMSSG,1020)FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      FLAG=' '
      WRITE(FLAG,810)2*OUSIZ(1)
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,1030)FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      FLAG='Disabled'
      IF(ISWAB.EQ.'YES ') FLAG='Enabled'
      WRITE(CMSSG,1040)FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      FLAG='Disabled'
      IF(ISWAH.EQ.'YES ') FLAG='Enabled'
      WRITE(CMSSG,1050)FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      FLAG='Disabled'
      IF(MOCMO.EQ.'UPON') FLAG='Enabled'
      WRITE(CMSSG,1060)FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      FLAG='Disabled'
      IF(IREBF.EQ.'YES ') FLAG='Enabled'
      WRITE(CMSSG,1070) FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      FLAG='Disabled'
      IF(LISFLG.EQ.'LON ') FLAG='Enabled'
      WRITE(CMSSG,1080) FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      FLAG=' '
      WRITE(FLAG,815)CLIDHI,CLIDLO
  815 FORMAT(I5,',',I5)
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,1110) FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(LFORM.NE.'L001') RETURN
C
      WRITE(FLAG,810)MAXIP
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,1090) FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(FLAG,810)NSKIP
      CALL SQUEZL(FLAG,1,12)
      WRITE(CMSSG,1100) FLAG
      CALL MESSLOG(LOGUT,LOGUP)
C
      RETURN
C
 1000 FORMAT('Input  data format is set to ----------- ',A4)
C
 1005 FORMAT('Output data format is set to ----------- ',A4)
C
 1010 FORMAT('Input data record-length is set to ----- ',A)
C
 1020 FORMAT('Input data auto-recl-detect is --------- ',A)
C
 1030 FORMAT('Output record-length is set to --------- ',A)
C
 1040 FORMAT('Input data byte-swap is ---------------- ',A)
C
 1050 FORMAT('Header byte-swap is -------------------- ',A)
C
 1060 FORMAT('Processing via calls to USERMOC is ----- ',A)
C
 1070 FORMAT('Calls to user-supplied REBUF is -------- ',A)
C
 1080 FORMAT('Output to lemor.log is ----------------- ',A)
C
 1090 FORMAT('L001 format - # parameters/event ------- ',A)
C
 1100 FORMAT('L001 format - # header words to skip --- ',A)
C
 1110 FORMAT('VME-clock Parameter IDs - Hi,Lo parts -- ',A)

C
C
C     ------------------------------------------------------------------
C     Display 1-D gates
C     ------------------------------------------------------------------
C
 1500 DO 1520 I=1,GATMX
      IF(GATLO(I).GT.GATHI(I)) GO TO 1520
      WRITE(CMSSG,1505)I,GATLO(I),GATHI(I)
 1505 FORMAT('Gate-ID, LO, HI =',3I6)
      CALL MESSLOG(LOGUT,LOGUP)
 1520 CONTINUE
      RETURN
      END
