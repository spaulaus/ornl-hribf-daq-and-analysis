C$PROG STATMAN   - Displays status information - open files, etc
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/03/99
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
      COMMON/SC00/ IHEPF 
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/SC02/ NAMH(20)
      INTEGER*4    NAMH
C     ------------------------------------------------------------------
      COMMON/SC04/ JCNF,IHEDN,MBFL
      INTEGER*4         IHEDN,MBFL
      CHARACTER*4  JCNF
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC12/ MEM_STYLE,SHMID
      CHARACTER*80 MEM_STYLE
      INTEGER*4    SHMID
C     ------------------------------------------------------------------
      COMMON/SC13/ LCON,LCMD,LIN,LBAN,LHEP
      INTEGER*4    LCON,LCMD,LIN,LBAN,LHEP
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
      cHARACTER*4  SCANTYP
C     ------------------------------------------------------------------
      COMMON/SC22/ IPCNAM(20),LDFNAM(20),TAPNAM(20),BANNAM(20)
      INTEGER*4    IPCNAM,    LDFNAM,    TAPNAM,    BANNAM
C     ------------------------------------------------------------------
      COMMON/SC23/ GATNAM(20),GATLO(1000),GATHI(1000),GATMX,GATOP,LUGAT
      INTEGER*4    GATNAM,    GATLO,      GATHI,      GATMX,GATOP,LUGAT
C     ------------------------------------------------------------------
      COMMON/SC27/ BNDX
      INTEGER*4    BNDX
C     ------------------------------------------------------------------
      COMMON/SC28/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC,CLONOF
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
      CHARACTER*4                                             CLONOF
C     ------------------------------------------------------------------
      COMMON/SC29/ UDFNAM(20),UDFRECL,UDFNPAR
      INTEGER*4    UDFNAM,    UDFRECL,UDFNPAR
C     ------------------------------------------------------------------
      INTEGER*4    STRLEN,I
C
      CHARACTER*4  KMX
C
      EQUIVALENCE (KMX,LWD(1,2))
C
      CHARACTER*80 CNAMH,CIPCNAM,CLDFNAM,CTAPNAM,CBANNAM,CUDFNAM
C
      EQUIVALENCE (CNAMH,NAMH),
     &            (CIPCNAM,IPCNAM),
     &            (CLDFNAM,LDFNAM),
     &            (CTAPNAM,TAPNAM),
     &            (CBANNAM,BANNAM),
     &            (CUDFNAM,UDFNAM)
C
      CHARACTER*20 SHARED,UNDEF,MEMTYP
      DATA         SHARED,UNDEF/'Shared Memory','Undefined'/
C
      CHARACTER*16  ITM
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KMX.EQ.'GATE') GO TO 500
C
      MEMTYP='SHARED'
      IF(MEM_STYLE.EQ.'LOCAL') MEMTYP='LOCAL'
      WRITE(CMSSG,90)MEMTYP
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,100) SCANTYP
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,110) LFORM
      CALL MESSLOG(LOGUT,LOGUP)
C
      ITM='Disabled'
      IF(LAUTO.EQ.'YES ') ITM='Enabled'
      WRITE(CMSSG,120) ITM
      CALL MESSLOG(LOGUT,LOGUP)
C
      ITM=' '
      WRITE(ITM,10)LNBY
   10 FORMAT(I8)
      CALL SQUEZL(ITM,1,8)
      WRITE(CMSSG,130) ITM
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,140) (CNAMH(1:STRLEN(CNAMH)))
      CALL MESSLOG(LOGUT,LOGUP)
C
C
      IF(INTYP.EQ.'TAPE') WRITE(CMSSG,150) (CTAPNAM(1:STRLEN(CTAPNAM)))
      IF(INTYP.EQ.'LDF ') WRITE(CMSSG,150) (CLDFNAM(1:STRLEN(CLDFNAM)))
      IF(INTYP.EQ.'UDF ') WRITE(CMSSG,150) (CUDFNAM(1:STRLEN(CUDFNAM)))
      IF(INTYP.EQ.'SHM ') WRITE(CMSSG,155) (CIPCNAM(1:STRLEN(CIPCNAM)))
      IF(INTYP.EQ.'    ') WRITE(CMSSG,150) UNDEF
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(INTYP.EQ.'UDF '.AND.UDFRECL.GT.0) THEN
      WRITE(CMSSG,152) UDFRECL
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(LBAN.GT.0) THEN
      WRITE(CMSSG,160) (CBANNAM(1:STRLEN(CBANNAM)))
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(ISWAB.EQ.'YES ') THEN
      ITM='Enabled'
      WRITE(CMSSG,190)ITM
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      ITM='Disabled'
      IF(LISFLG.EQ.'LON ') THEN
      ITM='Enabled'
      WRITE(CMSSG,200)ITM
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(LFORM.EQ.'L001') THEN
      CALL MESSLOG(LOGUT,LOGUP)
      ITM=' '
      WRITE(ITM,10)MAXIP
      CALL SQUEZL(ITM,1,8)
      WRITE(CMSSG,210)ITM
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(ITM,10)NSKIP
      CALL SQUEZL(ITM,1,8)
      WRITE(CMSSG,220)ITM
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(CLIDHI.GT.0) THEN
      WRITE(CMSSG,230)CLIDHI,CLIDLO
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(VMETHI.GT.0) THEN
      CALL VMETFOR1(VMETLO,ITM)
      WRITE(CMSSG,240)ITM
      CALL MESSLOG(LOGUT,LOGUP)
      CALL VMETFOR1(VMETHI,ITM)
      WRITE(CMSSG,250)ITM
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      WRITE(CMSSG,260)NCEOF
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,270)INRECI
      CALL MESSLOG(LOGUT,LOGUP)
C
C
   90 FORMAT('Histogram memory type ---------------- ',A)
  100 FORMAT('Scan mode ---------------------------- ',A4)
  110 FORMAT('Data format -------------------------- ',A4)
  120 FORMAT('Auto RECL detection ------------------ ',A)
  130 FORMAT('Current RECL is set to --------------- ',A)
  140 FORMAT('HIS-file Name ------------------------ ',A)
  150 FORMAT('Data input is from ------------------- ',A)
  152 FORMAT('UDF RECL (bytes) --------------------- ',I6)
  155 FORMAT('Data input is from ------------------- SHM - ',A)
  160 FORMAT('BAN-file Name ------------------------ ',A)
  190 FORMAT('Byte-swap of input data is ----------- ',A)
  200 FORMAT('Output to the log-file is ------------ ',A)
  210 FORMAT('L001 format - # parameters/event ----- ',A)
  220 FORMAT('L001 format - # header words to skip - ',A)
  230 FORMAT('VME 100 Hertz Clock Param IDs HI,LO -- ',2I6)
  240 FORMAT('VME 100 Hertz Clock LO-limit --------- ',A)
  250 FORMAT('VME 100 Hertz Clock HI-limit --------- ',A)
  260 FORMAT('No. of contiguous EOFs encountered --- ',I10)
  270 FORMAT('Next Rec# to be read from input file - ',I10)
C
      RETURN
C
C     ------------------------------------------------------------------
C     Display 1-D gates
C     ------------------------------------------------------------------
C
  500 DO 520 I=1,GATMX
      IF(GATLO(I).GT.GATHI(I)) GO TO 520
      WRITE(CMSSG,505)I,GATLO(I),GATHI(I)
  505 FORMAT('Gate-ID, LO, HI =',3I6)
      CALL MESSLOG(LOGUT,LOGUP)
  520 CONTINUE
      RETURN
      END
