C$PROG SCANORNIT - Initializes things for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/29/99
C     ******************************************************************
C
      SUBROUTINE SCANORNIT
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
      COMMON/SC00/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/SC01/ NAMCMD(20)
      INTEGER*4    NAMCMD
C     ------------------------------------------------------------------
      COMMON/SC02/ NAMH(20)
      INTEGER*4    NAMH
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
      COMMON/SC20/ SCANTYP
      CHARACTER*4  SCANTYP
C     ------------------------------------------------------------------
      CHARACTER*80 ARG1,ARG2,ARG3,NAMLOG
      EQUIVALENCE (ARG1,NAMH)
C     ------------------------------------------------------------------
      INTEGER*4    LENLOG,TERMINATE_STRING,IEXIST,IERR,NBYTHIS,I
C
      INTEGER*4    IARGC,NUMARG
C
      INTEGER*4    IHUP,IGET,IZOT
      DATA         IHUP,IGET,IZOT/'HUP ','GET ','ZOT '/
C
      INTEGER*4    LUT,LUH,LUD
      EQUIVALENCE (LUT,LUC(1)),(LUH,LUC(6)),(LUD,LUC(9))
C
      DATA         NAMPROG/'SCAN','MO  '/
C
      INTEGER*4    BLANK,CON
      DATA         BLANK,CON/'    ','CON:'/
C
      SAVE
C
C     ------------------------------------------------------------------
      LUC(1) = -1                  !CHAN# FOR MAG TAPE INPUT
      LUC(2) = -1                  !CHAN# FOR MAG TAPE INPUT
      LUC(3) =  0                  !NOT   USED
      LUC(4) =  0                  !NOT   USED
      LUC(5) =  0                  !NOT   USED
      LUC(6) = -1                  !CHAN# FOR HIS-FILE
      LUC(7) = -1                  !CHAN# FOR HIS-FILE
      LUC(8) =  0                  !NOT   USED
      LUC(9) =  9                  !LU#   FOR DRR-FILE
      LUC(10)= 10                  !LU#   FOR MIL-FILE
C
C     UNIT-4                       !INPUT  FROM BAN FILE
C     UNIT-5                       !INPUT  VDT (SYS$INPUT)
C     UNIT-6                       !OUTPUT VDT (SYS$OUTPUT)
C     UNIT-7                       !OUTPUT SCAN LOG-FILE
C     UNIT-8                       !INPUT  HEP-FILE
C     UNIT-9                       !INPUT  DRR-FILE
C     UNIT-10                      !INPUT  MIL-FILE
C     UNIT-14                      !INPUT  COMMAND-FILE
C
C     ------------------------------------------------------------------
C     SET UP THINGS AND READ IN DIRECTORY ETC
C     ------------------------------------------------------------------
C   
      CALL SHARED_WIPE
C
      CMSSG=' '
      MSGF ='    '
      LOGUT=6
      LOGUP=7
      LISFLG='LON '
C
      LCON=5
      LCMD=14
      LIN=5
      LBAN=4
      LHEP=8
C
      NBRED=0
      NBTOP=100000000
C
      ICNF ='NO  '
      JCNF ='NO  '
      ISWAB='NO  '
C
      LSTL=16384
      LNBY=32768
      NSKIP=0
      LFORM='L003'
C
      NCEOF=0
      LAUTO='YES '
      SCANTYP='USER'
      MEM_STYLE=' '
C
C     ------------------------------------------------------------------
C     GET FILENAME FROM COMMAND LINE USING GETARGS (F77) CALL
C     ------------------------------------------------------------------
C
      ARG1 = ' '
      ARG2 = ' '
      ARG3 = ' '
C
      NUMARG=IARGC()
C
      CALL GETARG(1,ARG1)                 !Get his-file name prefix
      NAMLOG = ARG1                       !Save it
C
      IF(NUMARG.GE.2) CALL GETARG(2,ARG2)
C
      IF(NUMARG.GE.3) CALL GETARG(3,ARG3)
C
      CALL CASEUP(ARG2)
      CALL CASEUP(ARG3)
C
      IF(ARG2.EQ.'CHIL' .OR.ARG3.EQ.'CHIL')  SCANTYP='CHIL'
C
*      IF(ARG2.EQ.'LOCAL'.OR.ARG3.EQ.'LOCAL') MEM_STYLE='LOCAL'
*     Cygwin is always local - memory management at all
      MEM_STYLE='LOCAL'
C
      LENLOG= TERMINATE_STRING(NAMLOG)
      IF(LENLOG.GT.0)THEN
      NAMLOG(LENLOG+1:)='.log'
      LENLOG=LENLOG+4
C
      OPEN(UNIT=LOGUP,FILE=NAMLOG(1:LENLOG),STATUS='UNKNOWN')
                     ENDIF
C
C     ------------------------------------------------------------------
C     BUILD AND/OR READ IN FILES APPROPRIATE FOR SCAN-TYPE SELECTED
C     ------------------------------------------------------------------
C
      IF(SCANTYP.EQ.'USER') THEN
      CALL DRREXAM(IEXIST)
      CALL DRRSUB(IEXIST)
      CALL FILOPENU
      CALL DRREAD(LUD)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,100)
  100 FORMAT('SCANU-MODE selected')
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
C
      IF(SCANTYP.EQ.'CHIL') THEN
      CALL FILOPENC
      CALL DRREAD(LUD)
      CALL INITU
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,110)
  110 FORMAT('CHIL-MODE selected')
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
C   
      WRITE(CMSSG,120)NAMH
  120  FORMAT('SCAN LOG FROM HIS-FILE - ',20A4)
      CALL MESSLOG(0,LOGUP)
C
C
C     ------------------------------------------------------------------
C     GET MEMORY FOR CREATION OF HISTOGRAMS
C     ------------------------------------------------------------------
C  
      NBYTHIS=2*NHWH
C
      IF((MEM_STYLE(1:5).NE.'LOCAL'))THEN
*        CALL SHM_GET_HISSPACE(NBYTHIS,SHMID,IERR) !use shared memory (default)
*        IF(IERR.EQ.0)THEN
*           CALL SHM_OPEN(SHMID,IERR)                 !Get address of the shm seg
*           WRITE(21,*) SHMID                         !Share the shared memory id
*           CLOSE(21)
*        ENDIF
      ELSE
         CLOSE(UNIT=21,STATUS='DELETE')
         CALL MEM_GET_HISSPACE(NBYTHIS,IERR)       !use local memory
      ENDIF
C
      IF(IERR.EQ.0)THEN
                   WRITE(LOGUT,130)NBYTHIS
                   ELSE
                   WRITE(LOGUT,140)NBYTHIS
                   WRITE(LOGUT,150)
                   STOP
                   ENDIF
C
  130 FORMAT(1H ,'REQUEST FOR ',I12,' BYTES OF HIS-MEMORY GRANTED')
  140 FORMAT(1H ,'REQUEST FOR ',I12,' BYTES OF HIS-MEMORY DENIED')
  150 FORMAT(1H ,'Sorry, this is fatal. Goodbye')
C
      CALL MEM_ZERO_HISSPACE(NBYTHIS,IERR)
C   
C     ------------------------------------------------------------------
C     INITIALIZE SOME THINGS INCLUDING HIS-FILE
C     ------------------------------------------------------------------
C
      IF(LUC(7).EQ.555)THEN             !If HIS file new (for VM version)
          LUC(7)=LUC(6)
          CALL HISNIT(LUH,IHUP)
      ELSE
          CALL HISNIT(LUH,IGET)         !If HIS already exists
      ENDIF
C
      CALL CTCNIT
C
      CALL STOPNIT
C   
C     ------------------------------------------------------------------
C   
      DO 160 I=1,20
      NAMCMD(I)=BLANK
  160 CONTINUE
      NAMCMD(1)=CON
C
      CALL HELPNIT(IHEPF)
C
C
      RETURN
      END
