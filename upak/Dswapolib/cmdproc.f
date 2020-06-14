C$PROG CMDPROC   - Command processor for program SWAPO
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE CMDPROC(IDONE,IERR)
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
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/BBB/ LUS,LUD,LUH,LUF
      DATA        LUS,LUD,LUH,LUF/3,1,2,3/
C     ------------------------------------------------------------------
      CHARACTER*4  KMD,IDONE,IX,EXT,JEX
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      INTEGER*4 NAM(20),NAMF(20),IDSK(2)
C
      INTEGER*4 RECLVALU
C   
      CHARACTER*80 CNAMF,CNAM
      EQUIVALENCE (CNAMF,NAMF(1)),(CNAM,NAM(1))
C
      character*4 cidsk(2)
      equivalence (cidsk, idsk)
      DATA cIDSK/2*'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
C
      IF(KMD.EQ.'LON ') GO TO 20
      IF(KMD.EQ.'LOF ') GO TO 20
C
      IF(KMD.EQ.'TEST') GO TO 50
      IF(KMD.EQ.'SWAP') GO TO 50
      GO TO 500
C
   20 LISFLG=KMD
      GO TO 1000
C
   50 EXT='    '                             !SET DEFAULT FILE EXTENT
C   
      CALL BILNAM(IWDRAW,IDSK,EXT,NAMF,IX,LD,NU,IERR) !GET FILNAM, EXT..
C   
      IF(IERR.NE.0)    GO TO 1000            !TST FOR ERROR
C   
      IF(IX.EQ.'.spk') GO TO 100             !TST FOR SPK-FILE
      IF(IX.EQ.'.drr') GO TO 200
      IF(IX.EQ.'.his') GO TO 300             !TST FOR HIS-FILE
      IF(IX.EQ.'.ldf') GO TO 400             !TST FOR LDF-FILE
C
      GO TO 500
C   
C     *************************************************************
C     OPEN SPK-FILE 
C     *************************************************************
C   
  100 CLOSE(UNIT=LUS)                        !CLOSE SPK-FILE
C
      OPEN(UNIT       = LUS,                 !OPEN SPK-FILE
     &     FILE       = CNAMF,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(2048),
     &     FORM       = 'UNFORMATTED',
     &     IOSTAT     = IOS)
C   
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 600
C   
      CALL SPKSWAP(KMD)                      !Swap Bytes
C   
      GO TO 1000
C  
C     *************************************************************
C     Open drr-file for I/O
C     *************************************************************
C  
  200 DO 210 I=1,20
      NAM(I)=NAMF(I)
  210 CONTINUE
C 
      IERR=0                                 !ERROR FLAG
      IF(LD.LE.0) GO TO 500
C 
      CLOSE(UNIT=LUD)                        !CLOSE DRR-FILE

      OPEN(UNIT       = LUD,                 !OPEN HIS-FILE FOR INP
     &     FILE       = CNAM,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     FORM       = 'UNFORMATTED',
     &     RECL       = RECLVALU(128),
     &     IOSTAT     = IOS)
C 
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 600
C
      CALL DRRSWAP(KMD)
C
      GO TO 1000
C   
C     *************************************************************
C     Open drr and his-files for I/O
C     *************************************************************
C   
  300 DO 310 I=1,20
      NAM(I)=NAMF(I)
  310 CONTINUE
C   
      IERR=0                                 !ERROR FLAG
      IF(LD.LE.0) GO TO 500
C
      CLOSE(UNIT=LUH)                        !CLOSE HIS-FILE
C   
      OPEN(UNIT       = LUH,                 !OPEN HIS-FILE FOR INP
     &     FILE       = CNAM,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     FORM       = 'UNFORMATTED',
     &     RECL       = RECLVALU(16384),
     &     IOSTAT     = IOS)
C   
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 600
      JEX='.drr'
      CALL LODUP(JEX,1,4,NAM,LD)             !SET .drr EXT IN FILNAM
C
      CLOSE(UNIT=LUD)                        !CLOSE DRR-FILE
C   
      OPEN(UNIT       = LUD,                 !OPEN DRR-FILE FOR INP
     &     FILE       = CNAM,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     FORM       = 'UNFORMATTED',
     &     RECL       = RECLVALU(128),
     &     IOSTAT     = IOS)
C   
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 600
C   
      IF(KMD.EQ.'SWAP') CALL HISSWAP(KMD)    !SWAP HIS- & DRR-FILES
C
      IF(KMD.EQ.'TEST') CALL DRRSWAP(KMD)    !Test compatility only
C   
      GO TO 1000
C
C     *************************************************************
C     Open ldf-file for I/O
C     *************************************************************
C
  400 CLOSE(UNIT=LUF)
C
      OPEN(UNIT      = LUF,
     &     FILE      = CNAMF,
     &     STATUS    = 'OLD',
     &     ACCESS    = 'DIRECT',
     &     RECL      = RECLVALU(32776),
     &     IOSTAT    = IOS)
C
      IF(IOS.NE.0) THEN
      CALL IOFERR(IOS)
      GO TO 600
      ENDIF
C
      CALL LDFSWAP(KMD)
C
      GO TO 1000
C   
C     *************************************************************
C     ERROR RETURNS
C     *************************************************************
C   
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error or illegal request - ignored')
      CALL MESSLOG(LOGUT,LOGUP)
C   
  600 IERR=1
C
 1000 IDONE='YES '
      RETURN
      END
